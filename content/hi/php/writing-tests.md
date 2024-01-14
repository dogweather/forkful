---
title:                "PHP: तेस्ट लिखना"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/writing-tests.md"
---

{{< edit_this_page >}}

"## क्यों"
टेस्ट कस्टमर ध्यान रखना आपके कोड की साइन्टेक ल को विश्वसनीय बनाता है और बग्स को पकड़ने में सहायता करता है। टेस्ट लिखना आपके कोड की सत्यता को बढ़ाता है और दोगुना सुनिश्चित करता है कि आपकी वेब एप्प सुरक्षित रहती है।

"## कैसे करने के लिए"
```PHP 
<?php
    // आपका कोड यहाँ लिखें
    function add($num1, $num2){
        return $num1 + $num2;
    }

    $result = add(2, 3);
    echo $result; // Output: 5
?>
```

ऑबजेक्ट ओरिएंटेड प्रोग्रामिंग में, हम महान और शानदार भारतीय शहर मुंबई से दो ऑबजेक्ट स्टार्ट बनाएँगे जो हमें टेस्ट करने में मदद करेंगे। यहाँ हम दो टेस्ट क्लास प्रस्तुत करेंगे: एक "स्टैक" और एक "कटा"। हम नोटसिल राखेंगे कि दोनों इन्हें स्टैक मैनेज़ करेंगे, जो कि एक चीज है जो फ़ोन्सदेसी (तालिबान) कोडतवा की सहायता से बनाई गई है।

```PHP 
<?php
    // स्टैक क्लास
    class Stack {
        private $items;

        public function __construct(){
            $this->items = array();
        }

        public function push($item){
            array_push($this->items, $item);
        }

        public function pop(){
            return array_pop($this->items);
        }
    }

    // कटा क्लास
    class Queue extends Stack{
        public function enqueue($item){
            $this->items[] = $item;
        }

        public function dequeue(){
            return array_shift($this->items);
        }
    }

    // लेट्स चलाना
    $stack = new Stack();
    $stack->push(10);
    $stack->push(20);
    echo $stack->pop(); // Output: 20

    $queue = new Queue();
    $queue->enqueue("abc");
    $queue->enqueue("xyz");
    echo $queue->dequeue(); // Output: abc
?>
```

"## गहराई में जाएँ"
टेस्ट कोडिंग का फैसला कोडिंग टेस्ट करेंगे कि हमारे पास अच्छी और सांप्रदायिक कोड या केवल काम करने वाला कोड या स्पेशल अपलोड