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
l.364:	# 12.000000
	.word	0x41400000
l.361:	# 11.000000
	.word	0x41300000
l.358:	# 10.000000
	.word	0x41200000
l.355:	# 9.000000
	.word	0x41100000
l.352:	# 8.000000
	.word	0x41000000
l.349:	# 7.000000
	.word	0x40e00000
l.346:	# 6.000000
	.word	0x40c00000
l.343:	# 5.000000
	.word	0x40a00000
l.340:	# 4.000000
	.word	0x40800000
l.337:	# 3.000000
	.word	0x40400000
l.334:	# 2.000000
	.word	0x40000000
l.331:	# 1.000000
	.word	0x3f800000
l.328:	# 0.000000
	.word	0x00000000
.text
.globl	main
main:
	sw	$ra, 4($sp)
	sw	$fp, 8($sp)
	addi	$sp, $sp, 24
	addi	$fp, $sp, 0
	li	$v0, 0
	l.sl	$f0, l.328
	sw	$ra, 0($sp)
	addi	$sp, $sp, 4
	jal	min_caml_create_float_array
	addi	$sp, $sp, -4
	lwr	$ra, 0($sp)
	addi	$a0, $v0, 0
	li	$v0, 2
	li	$v1, 3
	sw	$a0, 0($sp)
	sw	$ra, 4($sp)
	addi	$sp, $sp, 8
	jal	make.208
	addi	$sp, $sp, -8
	lwr	$ra, 4($sp)
	li	$v1, 3
	li	$a0, 2
	lwr	$a1, 0($sp)
	sw	$v0, 4($sp)
	addi	$v0, $v1, 0
	addi	$v1, $a0, 0
	addi	$a0, $a1, 0
	sw	$ra, 8($sp)
	addi	$sp, $sp, 12
	jal	make.208
	addi	$sp, $sp, -12
	lwr	$ra, 8($sp)
	li	$v1, 2
	li	$a0, 2
	lwr	$a1, 0($sp)
	sw	$v0, 8($sp)
	addi	$v0, $v1, 0
	addi	$v1, $a0, 0
	addi	$a0, $a1, 0
	sw	$ra, 12($sp)
	addi	$sp, $sp, 16
	jal	make.208
	addi	$sp, $sp, -16
	lwr	$ra, 12($sp)
	addi	$a3, $v0, 0
	lwr	$a1, 4($sp)
	lwr	$v0, 0($a1)
	l.sl	$f0, l.331
	s.s	$f0, 0($v0)
	lwr	$v0, 0($a1)
	l.sl	$f0, l.334
	s.s	$f0, 4($v0)
	lwr	$v0, 0($a1)
	l.sl	$f0, l.337
	s.s	$f0, 8($v0)
	lwr	$v0, 4($a1)
	l.sl	$f0, l.340
	s.s	$f0, 0($v0)
	lwr	$v0, 4($a1)
	l.sl	$f0, l.343
	s.s	$f0, 4($v0)
	lwr	$v0, 4($a1)
	l.sl	$f0, l.346
	s.s	$f0, 8($v0)
	lwr	$a2, 8($sp)
	lwr	$v0, 0($a2)
	l.sl	$f0, l.349
	s.s	$f0, 0($v0)
	lwr	$v0, 0($a2)
	l.sl	$f0, l.352
	s.s	$f0, 4($v0)
	lwr	$v0, 4($a2)
	l.sl	$f0, l.355
	s.s	$f0, 0($v0)
	lwr	$v0, 4($a2)
	l.sl	$f0, l.358
	s.s	$f0, 4($v0)
	lwr	$v0, 8($a2)
	l.sl	$f0, l.361
	s.s	$f0, 0($v0)
	lwr	$v0, 8($a2)
	l.sl	$f0, l.364
	s.s	$f0, 4($v0)
	li	$v0, 2
	li	$v1, 3
	li	$a0, 2
	sw	$a3, 12($sp)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	mul.186
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	lwr	$v0, 12($sp)
	lwr	$v1, 0($v0)
	l.sr	$f0, 0($v1)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	min_caml_truncate
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	min_caml_print_int
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	min_caml_print_newline
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	lwr	$v0, 12($sp)
	lwr	$v1, 0($v0)
	l.sr	$f0, 4($v1)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	min_caml_truncate
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	min_caml_print_int
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	min_caml_print_newline
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	lwr	$v0, 12($sp)
	lwr	$v1, 4($v0)
	l.sr	$f0, 0($v1)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	min_caml_truncate
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	min_caml_print_int
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	min_caml_print_newline
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	lwr	$v0, 12($sp)
	lwr	$v0, 4($v0)
	l.sr	$f0, 4($v0)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	min_caml_truncate
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	min_caml_print_int
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	sw	$ra, 16($sp)
	addi	$sp, $sp, 20
	jal	min_caml_print_newline
	addi	$sp, $sp, -20
	lwr	$ra, 16($sp)
	move	$sp, $fp
	subi	$sp, $sp, 24
	lwr	$ra, 4($sp)
	lwr	$fp, 8($sp)
	li	$v0, 0
	exit
loop3.140:
	li	$t0, 0
	bgt	$t0, $v1, ble_else.406
	sll	$t0, $v0, 2
	add	$t9, $a3, $t0
	lwr	$t0, 0($t9)
	sll	$t1, $v0, 2
	add	$t9, $a3, $t1
	lwr	$t1, 0($t9)
	sll	$t2, $a0, 2
	add	$t9, $t1, $t2
	l.sr	$f0, 0($t9)
	sll	$t1, $v0, 2
	add	$t9, $a1, $t1
	lwr	$t1, 0($t9)
	sll	$t2, $v1, 2
	add	$t9, $t1, $t2
	l.sr	$f1, 0($t9)
	sll	$t1, $v1, 2
	add	$t9, $a2, $t1
	lwr	$t1, 0($t9)
	sll	$t2, $a0, 2
	add	$t9, $t1, $t2
	l.sr	$f2, 0($t9)
	mul.s	$f1, $f1, $f2
	add.s	$f0, $f0, $f1
	sll	$t1, $a0, 2
	add	$t9, $t0, $t1
	s.s	$f0, 0($t9)
	addi	$v1, $v1, -1
	j	loop3.140
ble_else.406:
	jr	$ra
loop2.160:
	li	$t0, 0
	bgt	$t0, $a0, ble_else.408
	addi	$t0, $v1, -1
	sw	$a3, 0($sp)
	sw	$a2, 4($sp)
	sw	$a1, 8($sp)
	sw	$v1, 12($sp)
	sw	$v0, 16($sp)
	sw	$a0, 20($sp)
	addi	$v1, $t0, 0
	sw	$ra, 24($sp)
	addi	$sp, $sp, 28
	jal	loop3.140
	addi	$sp, $sp, -28
	lwr	$ra, 24($sp)
	lwr	$v0, 20($sp)
	addi	$a0, $v0, -1
	lwr	$v0, 16($sp)
	lwr	$v1, 12($sp)
	lwr	$a1, 8($sp)
	lwr	$a2, 4($sp)
	lwr	$a3, 0($sp)
	j	loop2.160
ble_else.408:
	jr	$ra
loop1.173:
	li	$t0, 0
	bgt	$t0, $v0, ble_else.410
	addi	$t0, $a0, -1
	sw	$a3, 0($sp)
	sw	$a2, 4($sp)
	sw	$a1, 8($sp)
	sw	$a0, 12($sp)
	sw	$v1, 16($sp)
	sw	$v0, 20($sp)
	addi	$a0, $t0, 0
	sw	$ra, 24($sp)
	addi	$sp, $sp, 28
	jal	loop2.160
	addi	$sp, $sp, -28
	lwr	$ra, 24($sp)
	lwr	$v0, 20($sp)
	addi	$v0, $v0, -1
	lwr	$v1, 16($sp)
	lwr	$a0, 12($sp)
	lwr	$a1, 8($sp)
	lwr	$a2, 4($sp)
	lwr	$a3, 0($sp)
	j	loop1.173
ble_else.410:
	jr	$ra
mul.186:
	addi	$v0, $v0, -1
	j	loop1.173
init.198:
	li	$a1, 0
	bgt	$a1, $v0, ble_else.412
	l.sl	$f0, l.328
	sw	$v1, 0($sp)
	sw	$a0, 4($sp)
	sw	$v0, 8($sp)
	addi	$v0, $v1, 0
	sw	$ra, 12($sp)
	addi	$sp, $sp, 16
	jal	min_caml_create_float_array
	addi	$sp, $sp, -16
	lwr	$ra, 12($sp)
	lwr	$v1, 8($sp)
	sll	$a0, $v1, 2
	lwr	$a1, 4($sp)
	add	$t9, $a1, $a0
	sw	$v0, 0($t9)
	addi	$v0, $v1, -1
	lwr	$v1, 0($sp)
	addi	$a0, $a1, 0
	j	init.198
ble_else.412:
	jr	$ra
make.208:
	sw	$v1, 0($sp)
	sw	$v0, 4($sp)
	addi	$v1, $a0, 0
	sw	$ra, 8($sp)
	addi	$sp, $sp, 12
	jal	min_caml_create_array
	addi	$sp, $sp, -12
	lwr	$ra, 8($sp)
	addi	$a0, $v0, 0
	lwr	$v0, 4($sp)
	addi	$v0, $v0, -1
	lwr	$v1, 0($sp)
	sw	$a0, 8($sp)
	sw	$ra, 12($sp)
	addi	$sp, $sp, 16
	jal	init.198
	addi	$sp, $sp, -16
	lwr	$ra, 12($sp)
	lwr	$v0, 8($sp)
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




