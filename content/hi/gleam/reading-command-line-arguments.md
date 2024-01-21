---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
date:                  2024-01-20T17:56:22.905732-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Command line arguments वे जानकारी हैं जो एक प्रोग्राम को शुरू करते समय user द्वारा दी जाती हैं। Programmers इन्हें इसलिए पढ़ते हैं ताकि वे अपने प्रोग्राम को flexible और user-friendly बना सकें।

## How to: (कैसे करें:)
```Gleam
import gleam/io
import gleam/list
import gleam/os.{Args}

pub fn main() {
  let Args(args) = os.args()
  match list.head(args) {
    Some(arg) -> 
      io.println("पहला आर्ग्युमेंट: " <> arg)
    None -> 
      io.println("कोई आर्ग्युमेंट प्रोवाइड नहीं किया गया।")
  }
}
```
जब आप ऊपर दिए गए प्रोग्राम को चलाएंगे, तो आपको command line पर पहला आर्ग्युमेंट दिखाई देगा अगर आपने कोई आर्ग्युमेंट दिया है।

## Deep Dive (गहन अध्ययन)
Command line arguments का इतिहास पुराने command-line interfaces तक जाता है जहाँ space-separated inputs को प्रोग्राम में parameters के रूप में दिया जा सकता था। Gleam में, `os.args()` function का इस्तेमाल करके हम user से inputs प्राप्त कर सकते हैं। Alternatives में आप libraries जैसे की `clap` या `structopt` का भी इस्तेमाल कर सकते हैं। लेकिन ज़्यादातर मामलों में, built-in `os` module पर्याप्त होती है।

## See Also (और जानकारी के लिए)
- [Gleam's Official Documentation for the os module](https://hexdocs.pm/gleam_stdlib/)
- [Command-line Arguments in Rust with clap crate](https://docs.rs/clap/)