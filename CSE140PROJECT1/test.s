addiu $t0, $t0, -4	# subtract 4 from $t0 (0xfffffffc)
addiu $t0, $t0, -4	# subtract 4 from $t0 (0xfffffff8)
addiu $t0, $t0, 12	# add 12 to $t0 (0x4)
addiu $t0, $t0, 12	# add 12 to $t0 (0x10)
addiu $t1, $0, 0x800	# create the lowest negative number
sll   $t1, $t1, 20	
slt   $t2, $t1, $0 	# this should be true
addiu $t1, $t1, -1	# create the highest positive number
slt   $t2, $t1, $0 	# this should be false
addi	$0,$0,0 #unsupported instruction, terminate
