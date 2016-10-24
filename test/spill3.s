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
	li	$v0, 0
	sw	$ra, 0($sp)
	addi	$sp, $sp, 4
	jal	f.56
	addi	$sp, $sp, -4
	lwr	$ra, 0($sp)
	sw	$ra, 0($sp)
	addi	$sp, $sp, 4
	jal	min_caml_print_int
	addi	$sp, $sp, -4
	lwr	$ra, 0($sp)
	move	$sp, $fp
	subi	$sp, $sp, 24
	lwr	$ra, 4($sp)
	lwr	$fp, 8($sp)
	li	$v0, 0
	exit
f.56:
	addi	$v1, $v0, 1
	addi	$a0, $v1, 1
	addi	$a1, $a0, 1
	addi	$a2, $a1, 1
	addi	$a3, $a2, 1
	addi	$t0, $a3, 1
	addi	$t1, $t0, 1
	addi	$t2, $t1, 1
	addi	$t3, $t2, 1
	addi	$t4, $t3, 1
	addi	$t5, $t4, 1
	addi	$t6, $t5, 1
	addi	$t7, $t6, 1
	addi	$s0, $t7, 1
	addi	$s1, $s0, 1
	addi	$s2, $s1, 1
	addi	$s3, $s2, 1
	addi	$s4, $s3, 1
	add	$s5, $s4, $v1
	add	$s6, $s5, $a0
	add	$s7, $s6, $a1
	add	$t8, $s7, $a2
	sw	$t8, 0($sp)
	add	$t8, $t8, $a3
	sw	$t8, 4($sp)
	add	$t8, $t8, $t0
	sw	$t8, 8($sp)
	add	$t8, $t8, $t1
	sw	$t8, 12($sp)
	add	$t8, $t8, $t2
	sw	$t8, 16($sp)
	add	$t8, $t8, $t3
	sw	$t8, 20($sp)
	add	$t8, $t8, $t4
	sw	$t8, 24($sp)
	add	$t8, $t8, $t5
	sw	$t8, 28($sp)
	add	$t8, $t8, $t6
	sw	$t8, 32($sp)
	add	$t8, $t8, $t7
	sw	$t8, 36($sp)
	add	$t8, $t8, $s0
	sw	$t8, 40($sp)
	add	$t8, $t8, $s1
	sw	$t8, 44($sp)
	add	$t8, $t8, $s2
	sw	$t8, 48($sp)
	add	$t8, $t8, $s3
	sw	$v0, 52($sp)
	add	$v0, $t8, $v0
	add	$v1, $v1, $a0
	add	$v1, $v1, $a1
	add	$v1, $v1, $a2
	add	$v1, $v1, $a3
	add	$v1, $v1, $t0
	add	$v1, $v1, $t1
	add	$v1, $v1, $t2
	add	$v1, $v1, $t3
	add	$v1, $v1, $t4
	add	$v1, $v1, $t5
	add	$v1, $v1, $t6
	add	$v1, $v1, $t7
	add	$v1, $v1, $s0
	add	$v1, $v1, $s1
	add	$v1, $v1, $s2
	add	$v1, $v1, $s3
	add	$v1, $v1, $s4
	add	$v1, $v1, $s5
	add	$v1, $v1, $s6
	add	$v1, $v1, $s7
	lwr	$a0, 0($sp)
	add	$v1, $v1, $a0
	lwr	$a0, 4($sp)
	add	$v1, $v1, $a0
	lwr	$a0, 8($sp)
	add	$v1, $v1, $a0
	lwr	$a0, 12($sp)
	add	$v1, $v1, $a0
	lwr	$a0, 16($sp)
	add	$v1, $v1, $a0
	lwr	$a0, 20($sp)
	add	$v1, $v1, $a0
	lwr	$a0, 24($sp)
	add	$v1, $v1, $a0
	lwr	$a0, 28($sp)
	add	$v1, $v1, $a0
	lwr	$a0, 32($sp)
	add	$v1, $v1, $a0
	lwr	$a0, 36($sp)
	add	$v1, $v1, $a0
	lwr	$a0, 40($sp)
	add	$v1, $v1, $a0
	lwr	$a0, 44($sp)
	add	$v1, $v1, $a0
	lwr	$a0, 48($sp)
	add	$v1, $v1, $a0
	add	$v1, $v1, $t8
	add	$v0, $v1, $v0
	lwr	$v1, 52($sp)
	add	$v0, $v0, $v1
	jr	$ra

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




