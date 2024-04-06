---
date: 2024-01-20 17:53:26.681303-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) ."
lastmod: '2024-04-05T21:53:54.466570-06:00'
model: gpt-4-1106-preview
summary: ''
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
