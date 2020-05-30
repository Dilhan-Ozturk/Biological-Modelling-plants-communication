The purpose of the study is to understand how plants can interact to communicate with each other.

An experimental part conducted by a group of American students (University of Georgia) tended to show that nearby bean plants exposed to different concentrations of nutrients can communicate through the root systems.

We used a modelling approach aimed at determining the vector of such a signal according to two main hypotheses: an active transport by mycorrhizae and a passive transport by diffusion of molecules in the medium.


In this repository, you will find :
- report.pdf : written report
- param.raparam and param.param : root architecture parameter set for CRootBox
- change_myco_growth_rate.R : Script to change the value of mycorrhizal growth rate and record the result (need of io_function.R) with CRootBox
- io_function.R : Used with previous script, allows to conduct a simulation suite where the growth rate of mycorrhizae varies for each simulation
- Get_time_before_mycorhize.R : establish the contacts between the two root systems
- Excudates_and_receptors.R : modelling of the matrcices of excudates and root receptors
- diffusion_model.R : diffusion model
