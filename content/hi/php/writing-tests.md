---
title:                "PHP: लिखना परीक्षाएं"
simple_title:         "लिखना परीक्षाएं"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि आपका कोड सही से काम कर रहा है या नहीं? आपको हर बार अपने कोड में बदलाव करने से पहले उसे चेक करने की आवश्यकता होती है। यहाँ हमारी मदद करती है अपने एपीचीआई के साथ प्रभावी तरीके से काम करो।

## कैसे करें

```PHP
<?php
use PHPUnit\Framework\TestCase;

class MathTest extends TestCase
{
    // कतिेता के लिए गुणन करें
    public function testMultiplication()
    {
        // थोड़े नमूने
        $this->assertEquals(4, 2*2);
        $this->assertEquals(12, 3*4);
        $this->assertEquals(8, 4*2);
        $this->assertEquals(20, 5*4);
    }

    // कतिेता के लिए विभाजन करें
    public function testDivision()
    {
        $this->assertEquals(2, 4/2);
        $this->assertEquals(3, 12/4);
        $this->assertEquals(4, 16/4);
    }
}
?>
```

उपरोक्त कोड का उपयोग करके, हम गुणा और विभाजन के लिए अलग-अलग फंक्शन बनाते हैं और उन्हें चेक करते हैं। इससे हम अपने एपीचीआई से सुनिश्चित कर सकते हैं कि हमारा कोड सही से काम कर रहा है या नहीं।

## गहराई से जाएं

जब हम टेस्ट कोड लिखते हैं, तो हम अपने कोड के अलावा भी अपने महत्वपूर्ण मानदंडों को समझते हैं। इससे हमारे कोड में निरंतर सुधार होते हैं और हमें अपने प्रोजेक्ट को बेहतर बनाने के लिए आगे बढ़ने की प्रेरणा मिलती है। टेस्ट कोड आपके एपीचीआई को भी सुरक्षित बनाता है क्योंकि वो आपके मानदंडों का पालन करता है।

## देखें भी

- PHPUnit: https://phpunit.de/ 
- इकाई टेस्ट के लिए PHPUnit शुरुआती गाइड: https://phpunit.readthedocs.io/en/9.1/index.html 
- एपीचीआई औ