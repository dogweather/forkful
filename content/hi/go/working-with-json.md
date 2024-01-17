---
title:                "जेसन के साथ काम करना"
html_title:           "Go: जेसन के साथ काम करना"
simple_title:         "जेसन के साथ काम करना"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/working-with-json.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON का उपयोग करना क्या है और प्रोग्रामर इसे क्यों करते हैं? JSON, जो जवास्क्रिप्ट ऑब्जेक्ट नोटेशन के शब्दों का एक शॉर्ट रूप है, एक प्रसिद्ध डेटा फॉर्मेट है जो कंप्यूटर हार्डवेयर और सॉफ्टवेयर के साथ काम करता है। अधिकांश संगठनों के डेटा को संग्रहीत करने और साझा करने के लिए JSON का उपयोग किया जाता है।

## कैसे करें:

JSON के साथ काम करने के लिए, Go प्रोग्रामिंग भाषा में कुछ सरल कोड के माध्यम से डेटा संचार और पार्सिंग किया जाता है। नीचे कुछ उदाहरण दिए गए हैं जो कि Go वायुमान में कोड की एक छोटी समझ का भाग हैं:

```
Go कोड नमूना:

// JSON डेटा को संशोधित करने के लिए डेटा संचार करें
func updateJSONData(data map[string]interface{}) error {
    
    // डेटा को JSON रूप में लोड करें
    jsonData, err := json.Marshal(data)
    if err != nil {
        return err
    }
    
    // कनवर्ट करें JSOON रूप से डेटा
    err = json.Unmarshal(jsonData, &data)
    if err != nil {
        return err
    }
    
    return nil
}

// डेटा को आउटपुट करने के लिए JSON फाइल तैयार करें
func writeToJSONFile(data map[string]interface{}) error {
    
    // JSON फाइल तैयार करें
    file, err := os.Create("output.json")
    if err != nil {
        return err
    }
    
    // डेटा को JSON रूप में लोड करें
    jsonData, err := json.Marshal(data)
    if err != nil {
        return err
    }
    
    // फाइल में डेटा लिखें
    _, err = file.Write(jsonData)
    if err != nil {
        return err
    }
    file.Close()
    
    return nil
}

// कार्यक्रम को चलाने के लिए इन कार्यों को बुलाएं
func main() {
    data := map[string]interface{}{"name": "John", "age": 25}
    updateJSONData(data)
    writeToJSONFile(data)
}
```

उपरोक्त कोड का आउटपुट output.json नाम के एक JSON फाइल में डेटा समायोजित करेगा:

```
output.json:

{
    "name": "John",
    "age": 25
}
```

## गहराई-छोटी:

JSON का अर्थ जवास्क्रिप्ट ऑब्जेक्ट नोटेशन के शब्दों का एक छोटा सा रूप है जो कंप्यूटर डेटा को संरचित और संग्रहीत करने के लिए डिज़ाइन किया गया है। यह विकसित होने के लिए १९९५ में और लेखिका डॉ. Douglas Crockford द्वारा जारी किया गया था। अल्टरनेटिव के रूप में, अन्य डेटा फॉर्मेट JSON जैसे CSV और XML है। Go में JSON को लोड और पार्स करने के लिए encoding/json पैकेज का उपयोग किया जाता है।

## साथ ही देख:

- [Go में जावास्क्रिप्ट ऑब्जेक्ट नोटेशन (JSON) का उपयोग कैसे करें](https://golang.org/pkg/encoding/json/)
- [JSON और XML के बीच अंतर](https://www