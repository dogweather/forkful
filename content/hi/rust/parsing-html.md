---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:34:33.278455-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"

category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTML पार्स करने से हमें वेब पेज के मार्कअप को समझने और उसे संसाधित करने का अवसर मिलता है। प्रोग्रामर्स इसे डेटा एकत्रित करने, वेबसाइट की स्थिति जांचने, और ऑटोमेटेड टेस्टिंग के लिए करते हैं।

## कैसे करें:

Rust में HTML को पार्स करने के लिए `scraper` क्रेट का उपयोग किया जाता है। यहाँ नमूना कोड है:

```Rust
use scraper::{Html, Selector};

fn main() {
    // HTML कोड जिसे पार्स करना है
    let html_content = r#"
        <html>
            <body>
                <p>Hello, world!</p>
            </body>
        </html>
    "#;

    // HTML पार्स करने के लिए
    let parsed_html = Html::parse_document(html_content);
    
    // `<p>` टैग्स को ढूंढने के लिए `Selector`
    let p_selector = Selector::parse("p").unwrap();
    
    // सभी `<p>` टैग्स को इटेरेट
    for element in parsed_html.select(&p_selector) {
        // एलिमेंट का टेक्स्ट कंटेंट प्रिंट करना
        println!("{}", element.inner_html().trim());
    }
}
```

सैंपल आउटपुट:

```
Hello, world!
```

## गहराई में:

HTML पार्सिंग वेब स्क्रेपिंग में जरूरी है, और ये वेब डेवलपमेंट के शुरुआती दिनों से ही की जा रही है। `scraper` क्रेट `html5ever` और `selectors` जैसे लाइब्रेरीज पर निर्मित है, जो तेज़ और लचीले हैं। वैकल्पिक रूप से, `beautifulsoup` जैसे उपकरण पाइथन में लोकप्रिय हैं, लेकिन Rust का `scraper` मेमोरी सेफ्टी और परफॉर्मेंस के अतिरिक्त लाभ प्रदान करता है। प्रत्येक एलिमेंट और नोड के साथ व्यावहारिक रूप से काम करने के लिए, `scraper` एक डोम बनाता है, जिससे ट्रीवाल्क करना आसान हो जाता है।

## संबंधित सूत्र:

- [html5ever GitHub रिपॉजिटरी](https://github.com/servo/html5ever)
- [Rust ऑफिशियल डॉक्यूमेंटेशन](https://doc.rust-lang.org/book/)

ध्यान देने योग्य है कि Rust जैसी कम लेवल वाली भाषाओं में पार्सिंग लॉजिक जटिल हो सकता है, लेकिन `scraper` जैसे उपकरणों की मदद से यह प्रक्रिया सरल और सुव्यवस्थित हो जाती है।
