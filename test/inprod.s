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
l.73:	# 6.000000
	.word	0x40c00000
l.72:	# 5.000000
	.word	0x40a00000
l.71:	# 4.000000
	.word	0x40800000
l.66:	# 3.000000
	.word	0x40400000
l.65:	# 2.000000
	.word	0x40000000
l.64:	# 1.000000
	.word	0x3f800000
l.63:	# 1000000.000000
	.word	0x49742400
.text
.globl	main
main:
	sw	$ra, 4($sp)
	sw	$fp, 8($sp)
	addi	$sp, $sp, 24
	addi	$fp, $sp, 0
	l.sl	$f0, l.63
	l.sl	$f1, l.64
	l.sl	$f2, l.65
	l.sl	$f3, l.66
	addi	$v0, $gp, 0
	addi	$gp, $gp, 12
	s.s	$f3, 8($v0)
	s.s	$f2, 4($v0)
	s.s	$f1, 0($v0)
	l.sl	$f1, l.71
	l.sl	$f2, l.72
	l.sl	$f3, l.73
	addi	$v1, $gp, 0
	addi	$gp, $gp, 12
	s.s	$f3, 8($v1)
	s.s	$f2, 4($v1)
	s.s	$f1, 0($v1)
	s.s	$f0, 0($sp)
	sw	$ra, 4($sp)
	addi	$sp, $sp, 8
	jal	inprod.38
	addi	$sp, $sp, -8
	lwr	$ra, 4($sp)
	l.sr	$f1, 0($sp)
	mul.s	$f0, $f1, $f0
	sw	$ra, 4($sp)
	addi	$sp, $sp, 8
	jal	min_caml_truncate
	addi	$sp, $sp, -8
	lwr	$ra, 4($sp)
	sw	$ra, 4($sp)
	addi	$sp, $sp, 8
	jal	min_caml_print_int
	addi	$sp, $sp, -8
	lwr	$ra, 4($sp)
	move	$sp, $fp
	subi	$sp, $sp, 24
	lwr	$ra, 4($sp)
	lwr	$fp, 8($sp)
	li	$v0, 0
	exit
getx.23:
	l.sr	$f0, 0($v0)
	mov.s	$f0, $f0
	jr	$ra
gety.28:
	l.sr	$f0, 4($v0)
	mov.s	$f0, $f0
	jr	$ra
getz.33:
	l.sr	$f0, 8($v0)
	mov.s	$f0, $f0
	jr	$ra
inprod.38:
	sw	$v0, 0($sp)
	sw	$v1, 4($sp)
	sw	$ra, 8($sp)
	addi	$sp, $sp, 12
	jal	getx.23
	addi	$sp, $sp, -12
	lwr	$ra, 8($sp)
	lwr	$v0, 4($sp)
	s.s	$f0, 8($sp)
	sw	$ra, 12($sp)
	addi	$sp, $sp, 16
	jal	getx.23
	addi	$sp, $sp, -16
	lwr	$ra, 12($sp)
	l.sr	$f1, 8($sp)
	mul.s	$f0, $f1, $f0
	lwr	$v0, 0($sp)
	s.s	$f0, 12($sp)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	gety.28
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	lwr	$v0, 4($sp)
	s.s	$f0, 16($sp)
	sw	$ra, 20($sp)
	addi	$sp, $sp, 24
	jal	gety.28
	addi	$sp, $sp, -24
	lwr	$ra, 20($sp)
	l.sr	$f1, 16($sp)
	mul.s	$f0, $f1, $f0
	l.sr	$f1, 12($sp)
	add.s	$f0, $f1, $f0
	lwr	$v0, 0($sp)
	s.s	$f0, 20($sp)
	sw	$ra, 24($sp)
	addi	$sp, $sp, 28
	jal	getz.33
	addi	$sp, $sp, -28
	lwr	$ra, 24($sp)
	lwr	$v0, 4($sp)
	s.s	$f0, 24($sp)
	sw	$ra, 28($sp)
	addi	$sp, $sp, 32
	jal	getz.33
	addi	$sp, $sp, -32
	lwr	$ra, 28($sp)
	l.sr	$f1, 24($sp)
	mul.s	$f0, $f1, $f0
	l.sr	$f1, 20($sp)
	add.s	$f0, $f1, $f0
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




