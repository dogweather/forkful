---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:27.837869-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Go \u092E\u0947\u0902\
  , \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B \u091C\
  \u094B\u0921\u093C\u0928\u0947 \u0915\u0947 \u0915\u0908 \u0924\u0930\u0940\u0915\
  \u0947 \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0902 \u0915\u0941\u091B \u0938\
  \u093E\u092E\u093E\u0928\u094D\u092F \u0924\u0930\u0940\u0915\u094B\u0902 \u092A\
  \u0930 \u090F\u0915 \u0928\u091C\u0930 \u0939\u0948 \u0909\u0926\u093E\u0939\u0930\
  \u0923\u094B\u0902 \u0915\u0947 \u0938\u093E\u0925."
lastmod: '2024-04-05T21:53:53.420508-06:00'
model: gpt-4-0125-preview
summary: "Go \u092E\u0947\u0902, \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\
  \u0938 \u0915\u094B \u091C\u094B\u0921\u093C\u0928\u0947 \u0915\u0947 \u0915\u0908\
  \ \u0924\u0930\u0940\u0915\u0947 \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0902\
  \ \u0915\u0941\u091B \u0938\u093E\u092E\u093E\u0928\u094D\u092F \u0924\u0930\u0940\
  \u0915\u094B\u0902 \u092A\u0930 \u090F\u0915 \u0928\u091C\u0930 \u0939\u0948 \u0909\
  \u0926\u093E\u0939\u0930\u0923\u094B\u0902 \u0915\u0947 \u0938\u093E\u0925."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B\
  \ \u091C\u094B\u0921\u093C\u0928\u093E"
weight: 3
---

##कैसे करें:
Go में, स्ट्रिंग्स को जोड़ने के कई तरीके हैं। यहां कुछ सामान्य तरीकों पर एक नजर है उदाहरणों के साथ:

### `+` ऑपरेटर का उपयोग करके:
स्ट्रिंग्स को जोड़ने का सबसे सरल तरीका `+` ऑपरेटर का उपयोग करना है। यह सीधा है लेकिन कई स्ट्रिंग्स के लिए सबसे कारगर नहीं है।
```go
firstName := "जॉन"
lastName := "डो"
fullName := firstName + " " + lastName
fmt.Println(fullName) // जॉन डो
```

### `fmt.Sprintf` का उपयोग करके:
वेरिएबल्स के साथ स्ट्रिंग्स को फॉर्मेट करने के लिए, `fmt.Sprintf` बहुत उपयोगी है। यह आउटपुट फॉर्मेट पर अधिक नियंत्रण देता है।
```go
age := 30
message := fmt.Sprintf("%s %d वर्ष के हैं.", fullName, age)
fmt.Println(message) // जॉन डो 30 वर्ष के हैं.
```

### `strings.Builder` का उपयोग करके:
विशेष रूप से लूप्स में, कई स्ट्रिंग्स को जोड़ने के लिए, `strings.Builder` कुशल और सुझाया गया है।
```go
var builder strings.Builder
words := []string{"हेलो", "वर्ल्ड", "से", "गो"}

for _, word := range words {
    builder.WriteString(word)
    builder.WriteString(" ")
}

result := builder.String()
fmt.Println(result) // हेलो वर्ल्ड से गो 
```

### `strings.Join` का उपयोग करके:
जब आपके पास विशेष विभाजक के साथ जुड़ने के लिए स्ट्रिंग्स की एक स्लाइस होती है, तो `strings.Join` सबसे अच्छा विकल्प है।
```go
elements := []string{"पथ", "से", "फ़ाइल"}
path := strings.Join(elements, "/")
fmt.Println(path) // पथ/से/फ़ाइल
```

## गहराई में जानकारी
स्ट्रिंग जोड़ना, जो एक सीधी सादी प्रक्रिया प्रतीत होती है, Go में स्ट्रिंग्स को कैसे संभाला जाता है, इसके गहरे पहलुओं को छूती है। Go में, स्ट्रिंग्स अपरिवर्तनीय होती हैं; अर्थात, हर जोड़ने की क्रिया एक नई स्ट्रिंग बनाती है। जब बड़ी संख्या में स्ट्रिंग्स को जोड़ने या इसे कसकर लूप्स में करने पर प्रदर्शन समस्याएं उत्पन्न हो सकती हैं, क्योंकि यह स्मृति की बार-बार आवंटन और प्रतिलिपि बनाने को लेकर होती है।

ऐतिहासिक रूप से, भाषाओं ने स्ट्रिंग अपरिवर्तनीयता और जोड़ने की कार्यकुशलता को विभिन्न तरीकों से संभाला है, और Go की `strings.Builder` और `strings.Join` के दृष्टिकोण प्रोग्रामर्स को उपयोग में आसानी और प्रदर्शन के संतुलन के साथ उपकरण प्रदान करते हैं। विशेष रूप से Go 1.10 में पेश किया गया `strings.Builder` प्रकार, कई स्ट्रिंग आवंटनों के ओवरहेड के बिना स्ट्रिंग्स को निर्माण करने का एक कुशल तरीका प्रदान करता है। यह ऐसा एक बफर आवंटित करके करता है जो जरूरत के अनुसार बढ़ता है, जिसमें स्ट्रिंग्स जोड़ी जाती हैं।

इन विकल्पों के बावजूद, प्रसंग के आधार पर सही विधि चुनना महत्वपूर्ण है। त्वरित या दुर्लभ जोड़ने के लिए, सरल ऑपरेटर्स या `fmt.Sprintf` पर्याप्त हो सकता है। हालांकि, प्रदर्शन-संवेदनशील पथों के लिए, विशेष रूप से जहां कई जोड़ने शामिल हैं, `strings.Builder` या `strings.Join` का उपयोग अधिक उपयुक्त हो सकता है।

जबकि Go स्ट्रिंग मैनिपुलेशन के लिए रॉबस्ट बिल्ट-इन क्षमताएं प्रदान करता है, अंतर्निहित प्रदर्शन विशेषताओं के प्रति सचेत रहना आवश्यक है। `+` या `fmt.Sprintf` के माध्यम से संयोजन जैसे विकल्प सादगी और छोटे पैमाने के संचालनों के लिए अच्छी तरह से सेवा करते हैं, लेकिन Go की अधिक कुशल स्ट्रिंग-निर्माण प्रथाओं को समझना और उपयोग करना सुनिश्चित करता है कि आपके अनुप्रयोग प्रदर्शनशील और स्केलेबल बने रहें।
