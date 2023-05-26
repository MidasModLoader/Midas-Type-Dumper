mod wiz_dumper;
use crate::wiz_dumper::dump_wiz_classes;

#[tokio::main]
async fn main() {
    dump_wiz_classes().await;
}
