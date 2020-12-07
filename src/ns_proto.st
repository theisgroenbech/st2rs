Problem: NS;
Principals: Alice, Bob;
Types: nonce, key, Enc<T>;
Functions: aenc<T>(T, key) -> Enc<T>, adec<T>(Enc<T>, key) -> T, pk(key) -> key;
Equations: adec(aenc(x,pk(y)),y) = x;
Formats:M1(key), M2(Enc<M2Inner>),M2Inner(key, nonce), M3(Enc<M3Inner>), M3Inner(nonce, nonce, key), M4(Enc<M4Inner>), M4Inner(nonce);

Protocol:

let Proto(A @ Alice, B @ Bob) =
Bob { new B: key; }
Bob -> Alice: a = M1<pk(B)>
Alice {
  let M1<pkB> = a;
  new A: key;
  new nA : nonce;
}
Alice -> Bob: x = M2<aenc(M2Inner<pk(A), nA>, pkB)>
Bob { 
  let M2<encrypted> = x;
  new nB: nonce;
  let M2Inner<pkA, nA> = adec(encrypted, B);
}
Bob -> Alice: y = M3<aenc(M3Inner<nA, nB, pk(B)>, pkA)>
Alice {
  let M3<encrypted> = y;
  let M3Inner<_nA, nB, _pkB> = adec(encrypted, A);
  let (%nA) = _nA;
  let (%pkB) = _pkB;
  event Secret(pk(A), pkB, nA);
}
Alice -> Bob: z = M4<aenc(M4Inner<nB>, pkB)>
Bob { 
  let M4<encrypted> = z;
  let M4Inner<_nB> = adec(encrypted, B);
  let (%nB) = _nB;
  event Secret(pkA, pk(B), nB);
}
end

in

Alice {}
Bob { }
Proto(A, B)
