---
title:                "डीबग आउटपुट प्रिंट करना"
aliases:
- /hi/php/printing-debug-output.md
date:                  2024-01-20T17:53:26.681303-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डीबग आउटपुट प्रिंट करने का मतलब है, कोड को रन करते समय वेरिएबल्स या कोड के बीच की प्रोसेसेस की जानकारी दिखाना ताकि समस्या का पता चल सके। प्रोग्रामर्स इसे इसलिए करते हैं ताकि उन्हें यह समझ में आ सके कि कोड किस तरह से बर्ताव कर रहा है और कहाँ सुधार की जरूरत है।

## How to: (कैसे करें)
```PHP
<?php
// सिंपल वेरिएबल डीबग
$variable = 'Hello, World!';
echo $variable; // Output: Hello, World!

// अर्रे का डीबग
$array = ['a' => 'Apple', 'b' => 'Banana'];
print_r($array);
/* Output:
Array
(
    [a] => Apple
    [b] => Banana
)
*/

// ऑब्जेक्ट्स के साथ काम करते समय डीबगिंग
class Fruit {
    public $name;
    public function __construct($name) {
        $this->name = $name;
    }
}

$fruit = new Fruit('Mango');
var_dump($fruit);
/* Output:
object(Fruit)#1 (1) {
  ["name"]=>
  string(5) "Mango"
}
*/
?>
```
## Deep Dive (गहराई में जानकारी)
PHP में डीबग आउटपुट का इस्तेमाल पुराने ज़माने से हो रहा है। `print_r()`, `var_dump()`, और `echo` जैसे फंक्शन आम तौर पर इस्तेमाल में आते हैं। वैकल्पिक तरीके जैसे कि Xdebug या IDE के बिल्ट-इन डीबगर भी हैं जो ज्यादा एडवांस डीबगिंग में मदद करते हैं। इंप्लीमेंटेशन डिटेल्स में, `var_dump()` कुछ ज्यादा जानकारी देता है, जैसे वेरिएबल के टाइप और साइज की जानकारी, जबकि `print_r()` का आउटपुट थोड़ा सरल होता है।


## See Also (और भी जानकारी)
- PHP Manual on `var_dump()`: https://www.php.net/manual/en/function.var-dump.php
- PHP Manual on `print_r()`: https://www.php.net/manual/en/function.print-r.php
- Debugging in PHP with Xdebug: https://xdebug.org/docs/
