
#Change to directory with simulated data
cd ../Simulated_data/Multicollinearity

#Run the WN sampler
for f in *WN*; do
	# run regression with k=1 and prior variance=5
	../../Ccodes_and_scripts/Regression_WN.exe 1 5 $f
	# run analysis with autocorrelations (argument 1) and write report to dat file
	../../Ccodes_and_scripts/Analysis.exe 1 output* Analysis_MulticollinearityWN.dat
	
	# zip the (large) output data
	zip ../../Simulation_output/Multicollinearity_out.zip output*
	zip ../../Simulation_output/Multicollinearity_raw.zip raw*
	zip ../../Simulation_output/Multicollinearity_analysis.zip analysis*
	zip ../../Simulation_output/Multicollinearity_autocor.zip auto*
	rm -f output*
	rm -f raw*
	rm -f analysis*
	rm -f auto*
done

mv Analys_MulticollinearityWN.dat ../../Simulation_output/

#Run the WC sampler
for f in *WC*; do
	# run regression with k=1 and prior variance=5
	../../Ccodes_and_scripts/Regression_WC.exe 1 5 $f
	# run analysis with autocorrelations (argument 1) and write report to dat file
	../../Ccodes_and_scripts/Analysis.exe 1 output* Analysis_MulticollinearityWC.dat
	
	# zip the (large) output data
	zip ../../Simulation_output/Multicollinearity_out.zip output*
	zip ../../Simulation_output/Multicollinearity_raw.zip raw*
	zip ../../Simulation_output/Multicollinearity_analysis.zip analysis*
	zip ../../Simulation_output/Multicollinearity_autocor.zip auto*
	rm -f output*
	rm -f raw*
	rm -f analysis*
	rm -f auto*
done

mv Analys_MulticollinearityWC.dat ../../Simulation_output/


#All output has been written to /Simulation_output/
#the Analysis_MulticollinearityWC.dat and Analysis_MulticollinearityWN.dat are important. Refer to the R code in \2 Simulation study\Diagnotic_tools_and_Analysis\Analysis.R for further instructions.

