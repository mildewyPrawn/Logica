(********************************************************************* 
       L\u00f3gica computacional 2018-2
       Pr\u00e1ctica 06: Coq- Deducci\u00f3n Natural e Inducci\u00f3n sobre listas.
       Selene Linares  
 **********************************************************************)

Section DeduccionNatural.

  Parameters ( q t p r s w:Prop)
             (A:Type)
             (a b c d e:A)
             (P Q F G T R': A -> Prop)
             (R : A -> A -> Prop).


  (****** Deducci\u00f3n Natural Proposicional ******)

  Theorem ExFalso: forall (x:Prop), False -> x.
  Proof.
    intros.
    elim H.
  Qed.

  Theorem ej1_a : (p -> q) -> (p -> r) -> (q -> r -> t) -> p -> t.
  Proof.
    intros.
    apply H1.
    apply H.
    trivial.
    apply H0.
    trivial.
  Qed.
  (*Admitted.*)

  Theorem proposicional_a : (p -> q) /\ (r -> s) -> p /\ r -> q /\ s.
  Proof.
    intros.
    split.
    destruct H.
    apply H.
    destruct H0.
    trivial.
    destruct H.
    apply H1.
    destruct H0.
    trivial.
  Qed.

  Theorem proposicional_b : (p -> q) -> (q -> r) -> ((p -> r) -> t -> q) -> ((p -> r) -> t) -> q.
  Proof.
    intros.
    apply H1.
    intros.
    apply H0.
    apply H.
    trivial.
    apply H2.
    intros.
    apply H0.
    apply H.
    trivial.
  Qed.

  Theorem proposicional_c : ((p /\ ~q) /\(p -> ~q)) -> ~(p -> q) /\ (q -> ~p).
  Proof.
    intros.
    destruct H.
    destruct H.
    split.
    unfold not.
    intros.
    apply H1 in H2.
    trivial.
    trivial.
    intros.
    unfold not.
    intros.
    absurd q.
    trivial.
    trivial.
  Qed.

  Theorem proposicional_d : ((p /\ q) /\ (r /\ ~s) /\ (q -> p -> t)) -> (t -> r -> s \/ w) -> w.
  Proof.
    intros.
    destruct H.
    destruct H.
    destruct H1.
    destruct H1.
    assert t.
    apply H3.
    trivial.
    trivial.
    trivial.
    assert (s \/ w).
    apply H0.
    trivial.
    trivial.
    destruct H6.
    assert (s \/ w).
    left.
    trivial.
    absurd s.
    trivial.
    trivial.
    trivial.
  Qed.

  (****** Deducci\u00f3n Natural Primer Orden *******)

  Theorem ej2_a: (exists x, P x \/ exists x, Q x) -> exists x, P x \/ Q x.
  Proof.
    intros.
    destruct H as [b].
    destruct H.
    exists b.
    left.
    trivial.
    destruct H as [c].
    exists c.
    right.
    trivial.
  Qed.
  (*Admitted.*)

  (*NOTA: cambiamos R x por R' x, ya que declaramos que R recibe dos par\u00e1metros, y en el PDF solo uno.*)
  Theorem primer_orden_a : (exists x, P x /\ Q c) /\ (forall x, P x -> R' x) -> (Q c /\ exists x, P x /\ R' x).
  Proof.
    intros.
    destruct H.
    destruct H.
    destruct H.
    split.
    trivial.
    exists x.
    split.
    trivial.
    apply H0.
    trivial.
  Qed.


  Theorem primer_orden_b : (forall x, G x -> P x \/ R' x) /\ (forall x, F x -> T x) -> ((forall x, P x \/ R' x -> F x) -> (forall x, G x -> T x)).
  Proof.
    intros.
    destruct H.
    apply H2.
    apply H0.
    apply H.
    trivial.
  Qed.


End DeduccionNatural.

(****** Inducci\u00f3n estructural sobre Listas ********)

Section InduccionListas.

  Require Import List.

  Require Import Bool.

  Require Import ZArith.

  Notation "x :: l" := (cons x l) (at level 60, right associativity).

  Notation "[ ]" := nil.

  Notation "[ x , .. , y ]" := (cons x .. (cons y nil) ..).


  Parameters (l l1 l2 : list A).

  Fixpoint longitud (l : list A) : nat :=
    match l with
    | nil => 0
    | (x::ls) => S(longitud (ls))
    end.

  Theorem longConc : forall (la lb:list A), longitud(la ++ lb) = longitud la + longitud lb.
  Proof.
    intros.
    induction la.
    simpl.
    reflexivity.
    simpl.
    rewrite IHla.
    reflexivity.
  Qed.

  (*Descomentar y completar definici\u00f3n de Reversa*)
  Fixpoint rev (l:list A): list A :=
    match l with
    | nil => nil
    | (x::xs) => rev xs++[x]
    end.

  Theorem conc_nil : forall(la :list A), la ++ [] = la.
  Proof.
    intros.
    induction la.
    simpl.
    reflexivity.
    simpl.
    rewrite IHla.
    reflexivity.
  Qed.

  Theorem con_tres :forall(la lb lc : list A), ((la ++ lb)++ lc) = (la ++ (lb ++ lc)).
  Proof.
  intros.
  induction la.
  simpl.
  reflexivity.
  simpl.
  rewrite IHla.
  reflexivity.
 Qed.

  Theorem revConc : forall (la lb :list A), rev(la ++ lb) = (rev lb) ++ (rev la).
  Proof.
  intros.
  induction la.
  destruct lb.
  simpl.
  trivial.
  simpl.
  rewrite conc_nil.
  reflexivity.
  simpl.
  rewrite IHla.
  rewrite con_tres.
  reflexivity.
  Qed.
  
  Theorem rev_list_a : forall (l : list A) (a:A), rev(l ++ [a]) = a :: rev l.
  Proof.
  intros.
  apply revConc.
  Qed.

  Theorem rev_rev : forall (la : list A), rev(rev la) = la.
  Proof.
  intros.
  induction la.
  simpl.
  reflexivity.
  simpl.
  rewrite rev_list_a.
  rewrite IHla.
  reflexivity.
  Qed.
  

End InduccionListas.


