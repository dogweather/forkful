---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:50.902257-07:00
description: "Kotlin \u092E\u0947\u0902 \u092A\u0930\u0940\u0915\u094D\u0937\u0923\
  \ (Tests) \u0932\u093F\u0916\u0928\u093E \u0906\u092A\u0915\u0947 \u0938\u0949\u092B\
  \u094D\u091F\u0935\u0947\u092F\u0930 \u092E\u0949\u0921\u094D\u092F\u0942\u0932\u094D\
  \u0938 \u0915\u0940 \u0915\u093E\u0930\u094D\u092F\u093E\u0924\u094D\u092E\u0915\
  \ \u0938\u0939\u0940\u0924\u093E \u0915\u094B \u0938\u094D\u0935\u091A\u093E\u0932\
  \u093F\u0924 \u0930\u0942\u092A \u0938\u0947 \u0938\u0924\u094D\u092F\u093E\u092A\
  \u093F\u0924 \u0915\u0930\u0928\u0947 \u0935\u093E\u0932\u0947 \u0915\u094B\u0921\
  \ \u0938\u094D\u0928\u093F\u092A\u0947\u091F\u094D\u0938 \u0915\u094B \u0924\u0948\
  \u092F\u093E\u0930 \u0915\u0930\u0928\u093E \u0936\u093E\u092E\u093F\u0932 \u0939\
  \u0948,\u2026"
lastmod: '2024-03-13T22:44:52.264774-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u092E\u0947\u0902 \u092A\u0930\u0940\u0915\u094D\u0937\u0923 (Tests)\
  \ \u0932\u093F\u0916\u0928\u093E \u0906\u092A\u0915\u0947 \u0938\u0949\u092B\u094D\
  \u091F\u0935\u0947\u092F\u0930 \u092E\u0949\u0921\u094D\u092F\u0942\u0932\u094D\u0938\
  \ \u0915\u0940 \u0915\u093E\u0930\u094D\u092F\u093E\u0924\u094D\u092E\u0915 \u0938\
  \u0939\u0940\u0924\u093E \u0915\u094B \u0938\u094D\u0935\u091A\u093E\u0932\u093F\
  \u0924 \u0930\u0942\u092A \u0938\u0947 \u0938\u0924\u094D\u092F\u093E\u092A\u093F\
  \u0924 \u0915\u0930\u0928\u0947 \u0935\u093E\u0932\u0947 \u0915\u094B\u0921 \u0938\
  \u094D\u0928\u093F\u092A\u0947\u091F\u094D\u0938 \u0915\u094B \u0924\u0948\u092F\
  \u093E\u0930 \u0915\u0930\u0928\u093E \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\
  ,\u2026"
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

Kotlin में परीक्षण (Tests) लिखना आपके सॉफ्टवेयर मॉड्यूल्स की कार्यात्मक सहीता को स्वचालित रूप से सत्यापित करने वाले कोड स्निपेट्स को तैयार करना शामिल है, यह सुनिश्चित करना कि वे अपेक्षित रूप से काम कर रहे हैं। प्रोग्रामर इसे बग्स को जल्दी पकड़ने, कोड रिफैक्टरिंग को सुविधाजनक बनाने, और यह प्रलेखन प्रदान करने के लिए करते हैं कि सॉफ्टवेयर घट

का कैसे काम करने का इरादा है।

## कैसे:

Kotlin विभिन्न फ्रेमवर्क्स के साथ टेस्ट-ड्रिवेन डेवेलपमेंट का समर्थन करता है, सबसे लोकप्रिय JUnit, Kotest, और मॉक्किंग के लिए MockK हैं। यहाँ JUnit का उपयोग करके एक सरल उदाहरण दिया गया है:

```कोटलिन
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CalculatorTest {

    @Test
    fun `दो संख्याओं का जोड़`() {
        val calculator = Calculator()
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }
}

class Calculator {
    fun add(a: Int, b: Int): Int = a + b
}
```

**नमूना आउटपुट**

```पाठ
Test passed.
```

Kotest का उपयोग करके अधिक सोफिस्टिकेटेड परीक्षण दृष्टिकोण के लिए, जो कोटलिन में एक अधिक मौलिक परीक्षण लिखने के शैली प्रदान करता है, नीचे दिए गए उदाहरण को देखें:

```कोटलिन
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CalculatorSpec : StringSpec({
    "2 और 3 को जोड़ने पर 5 आना चाहिए" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }
})
```

मॉक्क्स के साथ परीक्षण के लिए MockK का उपयोग करना:

```कोटलिन
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ServiceTest {

    private val repository = mockk<Repository>()
    private val service = Service(repository)

    @Test
    fun `get data returns mocked data`() {
        every { repository.getData() } returns "Mocked Data"

        val result = service.getData()

        assertEquals("Mocked Data", result)
    }
}

class Service(private val repository: Repository) {
    fun getData(): String = repository.getData()
}

interface Repository {
    fun getData(): String
}
```

**नमूना आउटपुट**

```पाठ
Test passed.
```

ये उदाहरण Kotlin में यूनिट परीक्षण लिखने की मूल बातें बताते हैं। जैसे-जैसे आपका एप्लिकेशन बढ़ता है, प्रत्येक फ्रेमवर्क द्वारा प्रदान की गई अधिक उन्नत परीक्षण तकनीकों और उपकरणों की खोज करने पर विचार करें।
