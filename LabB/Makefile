# two_%_i32s :
# 	futhark dataset -b --i32-bounds=-10000:10000 \
# 	-g [$*]i32 -g [$*]i32 > $@

# SIZES = 100 1000 10000 100000 1000000 5000000 10000000

# exercise1-opencl.json : $(SIZES:%=two_%_i32s) exercise1.fut
# 	futhark bench --backend=opencl \
# 				  --json exercise1-opencl.json \
# 					exercise1.fut

# exercise1-c.json : $(SIZES:%=two_%_i32s) exercise1.fut
# 	futhark bench --backend=c \
# 				  --json exercise1-c.json \
# 					exercise1.fut


.PHONY: data clean plot all

all: plot

plot: exercise_1_1.pdf exercise_1_4.pdf

two_%_i32s:
	futhark dataset -b --i32-bounds=-10000:10000 \
	                -g [$*]i32 -g [$*]i32 > $@

one_%_i32s: 
	futhark dataset -b --i32-bounds=-10000:10000 \
					-g [$*]i32 -g [$*]bool > $@

exercise_1_1.pdf: benchmark
	python3 plot-1.py exercise_1_1 $(SIZES)

exercise_1_4.pdf: benchmark
	python3 plot-1.py exercise_1_4 $(SIZES)

exercise_2_2.pdf: benchmark 
	python3 plot-2.py exercise_2_2 $(SIZES)

benchmark:  exercise_1_1-multi.json exercise_1_1-c.json \
			exercise_1_4-multi.json exercise_1_4-c.json \
			exercise_2_2-multi.json exercise_2_2-c.json

SIZES = 100 1000 10000 100000 1000000 5000000 10000000

exercise_1_1-multi.json: $(SIZES:%=two_%_i32s) exercise_1_1.fut
	futhark bench --backend=multicore \
	              --json exercise_1_1-multi.json \
	              exercise_1_1.fut

# exercise_1_1-opencl.json: $(SIZES:%=two_%_i32s) exercise_1_1.fut
# 	futhark bench --backend=opencl \
# 	              --json exercise_1_1-opencl.json \
# 	              exercise_1_1.fut

exercise_1_1-c.json: $(SIZES:%=two_%_i32s) exercise_1_1.fut
	futhark bench --backend=c \
	              --json exercise_1_1-c.json \
	              exercise_1_1.fut

exercise_1_4-multi.json: $(SIZES:%=two_%_i32s) exercise_1_4.fut
	futhark bench --backend=multicore exercise_1_4.fut --json exercise_1_4-multi.json

exercise_1_4-c.json: $(SIZES:%=two_%_i32s) exercise_1_4.fut
	futhark bench --backend=c exercise_1_4.fut --json exercise_1_4-c.json

exercise_2_2-c.json: $(SIZES:%=one_%_i32s) exercise_2_2.fut
	futhark bench --backend=c exercise_2_2.fut --json exercise_2_2-c.json

exercise_2_2-multi.json: $(SIZES:%=one_%_i32s) exercise_2_2.fut
	futhark bench --backend=multicore exercise_2_2.fut --json exercise_2_2-multi.json

clean:
	rm -f two_*_i32s *.json *.pdf *.bin *.c




