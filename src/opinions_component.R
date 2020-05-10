#change in opinions on climate chnage, given distribution of opinions, social network (and climate information)

opinionchange=function(distribution=c(frac_opp_0,frac_neut_0,frac_supp_0),evidence_t_1,evidence_effect=evidenceeffect,selfsimparams=homophily,force=force_params,policychange_t_1=0,policyopinionfeedback=policyopinionfeedback_param){
  network=networkfunc(distribution,selfsimparams)
  feltforce=network*force
  
  policy_effect=policyopinionfeedback*policychange_t_1 #opinion change from institutional norm signalling - off if policyopinionfeedbac==0
  
  force_opp_neut=min(max(sum(feltforce[1,2:3])+evidence_effect*evidence_t_1[1]+policy_effect,0),1)
  force_neut_opp=min(max(feltforce[2,1]-evidence_effect*evidence_t_1[2]-policy_effect,0),1)
  force_neut_supp=min(max(feltforce[2,3]+evidence_effect*evidence_t_1[2]+policy_effect,0),1)
  force_supp_neut=min(max(sum(feltforce[3,1:2])-evidence_effect*evidence_t_1[3]-policy_effect,0),1)
  
  transitionprobs=matrix(c(1-force_opp_neut,force_opp_neut,0,force_neut_opp,1-(force_neut_supp+force_neut_opp),force_neut_supp,0,force_supp_neut,1-force_supp_neut),byrow=T,nrow=3)
  
  return(distribution%*%transitionprobs)
}




