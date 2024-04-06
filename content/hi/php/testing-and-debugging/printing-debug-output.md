---
date: 2024-01-20 17:53:26.681303-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) PHP \u092E\
  \u0947\u0902 \u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u0915\
  \u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u092A\u0941\u0930\u093E\
  \u0928\u0947 \u095B\u092E\u093E\u0928\u0947 \u0938\u0947 \u0939\u094B \u0930\u0939\
  \u093E \u0939\u0948\u0964 `print_r()`, `var_dump()`, \u0914\u0930 `echo` \u091C\u0948\
  \u0938\u0947 \u092B\u0902\u0915\u094D\u0936\u0928 \u0906\u092E \u0924\u094C\u0930\
  \ \u092A\u0930 \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u092E\u0947\u0902\
  \ \u0906\u0924\u0947\u2026"
lastmod: '2024-04-05T22:51:07.161962-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) PHP \u092E\u0947\u0902\
  \ \u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u0915\u093E \u0907\
  \u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u092A\u0941\u0930\u093E\u0928\u0947\
  \ \u095B\u092E\u093E\u0928\u0947 \u0938\u0947 \u0939\u094B \u0930\u0939\u093E \u0939\
  \u0948\u0964 `print_r()`, `var_dump()`, \u0914\u0930 `echo` \u091C\u0948\u0938\u0947\
  \ \u092B\u0902\u0915\u094D\u0936\u0928 \u0906\u092E \u0924\u094C\u0930 \u092A\u0930\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u092E\u0947\u0902 \u0906\u0924\
  \u0947 \u0939\u0948\u0902\u0964 \u0935\u0948\u0915\u0932\u094D\u092A\u093F\u0915\
  \ \u0924\u0930\u0940\u0915\u0947 \u091C\u0948\u0938\u0947 \u0915\u093F Xdebug \u092F\
  \u093E IDE \u0915\u0947 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u0921\u0940\
  \u092C\u0917\u0930 \u092D\u0940 \u0939\u0948\u0902 \u091C\u094B \u091C\u094D\u092F\
  \u093E\u0926\u093E \u090F\u0921\u0935\u093E\u0902\u0938 \u0921\u0940\u092C\u0917\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092E\u0926\u0926 \u0915\u0930\u0924\u0947\
  \ \u0939\u0948\u0902\u0964 \u0907\u0902\u092A\u094D\u0932\u0940\u092E\u0947\u0902\
  \u091F\u0947\u0936\u0928 \u0921\u093F\u091F\u0947\u0932\u094D\u0938 \u092E\u0947\
  \u0902, `var_dump()` \u0915\u0941\u091B \u091C\u094D\u092F\u093E\u0926\u093E \u091C\
  \u093E\u0928\u0915\u093E\u0930\u0940 \u0926\u0947\u0924\u093E \u0939\u0948, \u091C\
  \u0948\u0938\u0947 \u0935\u0947\u0930\u093F\u090F\u092C\u0932 \u0915\u0947 \u091F\
  \u093E\u0907\u092A \u0914\u0930 \u0938\u093E\u0907\u091C \u0915\u0940 \u091C\u093E\
  \u0928\u0915\u093E\u0930\u0940, \u091C\u092C\u0915\u093F `print_r()` \u0915\u093E\
  \ \u0906\u0909\u091F\u092A\u0941\u091F \u0925\u094B\u0921\u093C\u093E \u0938\u0930\
  \u0932 \u0939\u094B\u0924\u093E \u0939\u0948\u0964."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

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
