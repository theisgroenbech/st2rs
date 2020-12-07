use std::marker::PhantomData;
use session_types::*;
use bytevec::*;

struct M1(i32, i32);

pub struct Repr<T>(Vec<u8>, PhantomData<T>);
//
// impl<T> Repr<T> {
//     fn new(data: T){
//         let vec = bytes::data
//         Self(data, PhantomData<T>)
//     }
// }

trait Bytes<T> {
    fn from_bytes(&self,_: Repr<T>) -> T;
    fn to_bytes(&self) -> Repr<T>;
}

impl Bytes<M1> {
    fn from_bytes(&self, b: Repr<M1>) -> M1 {
        let vec = <Vec<M1>>::decode::<u32>(&b).unwrap();
        vec[0]
    }
    fn to_bytes(&self) -> Repr<M1> {
        let slice = &[t];
        let bytes = slice.encode::<u32>().unwrap();
        Repr(bytes, PhantomData<M1>)
    }
}

type alice = Send<Repr<M1>, Recv<Repr<M1>, Eps>>;
type bob = Recv<Repr<M1>, Send<Repr<M1>, Eps>>;

fn main() {
    // let (alice_chan, bob_chan) = session_channel();
    // let alice_t = thread::spawn(move || alice(alice_chan, &Functions {}));
    // let bob_t = thread::spawn(move || bob(bob_chan, &Functions {}));
    // let _ = (alice_t.join(), bob_t.join());
    let M1(a,b) = M1(1,1);
}
