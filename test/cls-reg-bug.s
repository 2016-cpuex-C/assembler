.data
const_f_zero:
	.word	0x00000000
const_f_zero_neg:
	.word	0x80000000
const_f_one:
	.word	0x3f800000
const_f_two:
	.word	0x40000000
const_f_half:
	.word	0x3f000000
const_pi:
	.word	0x40490fdb
const_half_pi:
	.word	0x3fc90fdb
.text
.globl	main
main:
	sw	$ra, 4($sp)
	sw	$fp, 8($sp)
	addi	$sp, $sp, 24
	addi	$fp, $sp, 0
	li	$v0, 1
	li	$v1, 2
	li	$a0, 3
	li	$a1, 4
	li	$a2, 5
	li	$a3, 6
	li	$t0, 7
	li	$t1, 8
	li	$t2, 9
	li	$t3, 10
	addi	$t4, $gp, 0
	addi	$gp, $gp, 40
	sw	$t3, 36($t4)
	sw	$t2, 32($t4)
	sw	$t1, 28($t4)
	sw	$t0, 24($t4)
	sw	$a3, 20($t4)
	sw	$a2, 16($t4)
	sw	$a1, 12($t4)
	sw	$a0, 8($t4)
	sw	$v1, 4($t4)
	sw	$v0, 0($t4)
	addi	$v0, $t4, 0
	sw	$ra, 0($sp)
	addi	$sp, $sp, 4
	jal	h.26
	addi	$sp, $sp, -4
	lwr	$ra, 0($sp)
	sw	$ra, 0($sp)
	addi	$sp, $sp, 4
	jal	min_caml_print_int
	addi	$sp, $sp, -4
	lwr	$ra, 0($sp)
	sw	$ra, 0($sp)
	addi	$sp, $sp, 4
	jal	min_caml_print_newline
	addi	$sp, $sp, -4
	lwr	$ra, 0($sp)
	move	$sp, $fp
	subi	$sp, $sp, 24
	lwr	$ra, 4($sp)
	lwr	$fp, 8($sp)
	li	$v0, 0
	exit
g.38:
	lwr	$v1, 40($t8)
	lwr	$a0, 36($t8)
	lwr	$a1, 32($t8)
	lwr	$a2, 28($t8)
	lwr	$a3, 24($t8)
	lwr	$t0, 20($t8)
	lwr	$t1, 16($t8)
	lwr	$t2, 12($t8)
	lwr	$t3, 8($t8)
	lwr	$t4, 4($t8)
	add	$t2, $t4, $t2
	add	$t1, $t2, $t1
	add	$t0, $t1, $t0
	add	$a3, $t0, $a3
	add	$a2, $a3, $a2
	add	$a1, $a2, $a1
	add	$a0, $a1, $a0
	add	$v1, $a0, $v1
	add	$v1, $v1, $t3
	li	$a0, 0
	bgt	$v0, $a0, ble_else.90
	neg	$v0, $v0
	lwr	$s7, 0($t8)
	jr	$s7
ble_else.90:
	addi	$v0, $v1, 0
	jr	$ra
h.26:
	lwr	$v1, 36($v0)
	lwr	$a0, 32($v0)
	lwr	$a1, 28($v0)
	lwr	$a2, 24($v0)
	lwr	$a3, 20($v0)
	lwr	$t0, 16($v0)
	lwr	$t1, 12($v0)
	lwr	$t2, 8($v0)
	lwr	$t3, 4($v0)
	lwr	$v0, 0($v0)
	addi	$t8, $gp, 0
	addi	$gp, $gp, 44
	la	$t4, g.38
	sw	$t4, 0($t8)
	sw	$a0, 40($t8)
	sw	$a1, 36($t8)
	sw	$a2, 32($t8)
	sw	$a3, 28($t8)
	sw	$t0, 24($t8)
	sw	$t1, 20($t8)
	sw	$t2, 16($t8)
	sw	$t3, 12($t8)
	sw	$v1, 8($t8)
	sw	$v0, 4($t8)
	li	$v0, 1
	lwr	$s7, 0($t8)
	jr	$s7

##################
## libmincaml.s ##
##################

min_caml_print_newline:
	li	$v0, 10
	print_c	$v0
	jr	$ra

min_caml_print_int: # $v0
	print_i	$v0
	jr	$ra

min_caml_print_char: # $v0
	print_c	$v0
	jr	$ra

min_caml_print_float:
	print_f	$f0
	jr	$ra

min_caml_read_int:
	read_i	$v0
	jr	$ra

min_caml_read_float:
	read_f	$f0
	jr	$ra


min_caml_create_array: # array of length $v0, initialized by $v1
	move	$a0, $v0
	move	$v0, $gp
create_array_loop:
	li	$a2, 0
	bne	$a0, $a2, create_array_cont
create_array_exit:
	jr	$ra
create_array_cont:
	sw	$v1, 0($gp)
	addi	$a0, $a0, -1
	addi	$gp, $gp, 4
	j	create_array_loop
#
min_caml_create_float_array: # array of length $v0, initialized by $f0
	move	$a0, $v0
	move	$v0, $gp
create_float_array_loop:
	li	$a2, 0
	bne	$a0, $a2, create_float_array_cont
create_float_array_exit:
	jr	$ra
create_float_array_cont:
	s.s	$f0, 0($gp)
	addi	$a0, $a0, -1
	addi	$gp, $gp, 4
	j	create_float_array_loop

min_caml_truncate:
	j	min_caml_int_of_float
min_caml_int_of_float: # f0 -> v0
	ftoi	$v0, $f0
	jr	$ra

min_caml_float_of_int: # v0 -> f0
	itof	$f0, $v0
	jr	$ra

min_caml_floor: # f0 -> f0
	floor $f0, $f0
	jr	$ra

min_caml_sqrt: # f0 -> f0
	sqrt	$f0, $f0
	jr	$ra

min_caml_fiszero: # f0 -> v0
	l.sl	$f1, const_f_zero
	c.eq.s	$f0, $f1, min_caml_fiszero_t
	l.sl	$f1, const_f_zero_neg
	c.eq.s	$f0, $f1, min_caml_fiszero_t
	li	$v0, 0 # false
	jr	$ra
min_caml_fiszero_t:
	li	$v0, 1 # true
	jr	$ra

min_caml_fispos: # f0 -> v0
	l.sl	$f1, const_f_zero
	c.lt.s	$f1, $f0, min_caml_fispos_t
	li	$v0, 0 # false
	jr	$ra
min_caml_fispos_t:
	li	$v0, 1 # true
	jr	$ra

min_caml_fisneg: # f0 -> v0
	l.sl	$f1, const_f_zero
	c.lt.s	$f0, $f1, min_caml_fispos_t
	li	$v0, 0 # false
	jr	$ra
min_caml_fisneg_t:
	li	$v0, 1 # true
	jr	$ra

min_caml_xor: # v0, v1 -> v0
	li	$a0, 0
	beq	$v0, $a0, min_caml_xor_2
	# v0=t
	beq	$v1, $a0, min_caml_xor_1
	# v0=t, v1=t
	li	$v0, 0
	jr	$ra
min_caml_xor_1:
	# v0=t, v1=f
	li	$v0, 1
	jr	$ra
min_caml_xor_2:
	# v0=f
	beq	$v1, $a0, min_caml_xor_3
	# v0=f, v1=t
	li	$v0, 1
	jr	$ra
min_caml_xor_3:
	# v0=f, v1=f
	li	$v0, 0
	jr	$ra

min_caml_fless: # f0,f1 -> v0
	c.lt.s	$f0, $f1, min_caml_fless_true
	li	$v0, 0
	jr	$ra
min_caml_fless_true:
	li	$v0, 1
	jr	$ra

min_caml_fneg: # f0 -> f0
	l.sl	$f1, const_f_zero
	sub.s	$f0, $f1, $f0
	jr	$ra

min_caml_fabs: # f0 -> f0
	l.sl	$f1, const_f_zero
	c.lt.s	$f0, $f1, min_caml_fneg
	jr	$ra

min_caml_fsqr: # f0 -> f0
	mul.s	$f0, $f0, $f0
	jr	$ra

min_caml_fhalf: # f0 -> f0
	l.sl	$f1, const_f_half
	mul.s	$f0, $f0, $f1
	jr	$ra

###############
### sin, cos ##
###############

min_caml_sin:
	sin	$f0, $f0
	jr	$ra
min_caml_cos:
	cos	$f0, $f0
	jr	$ra
min_caml_atan:
	atan	$f0, $f0
	jr	$ra




