---
title:                "JSON के साथ काम करना"
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON (JavaScript Object Notation) एक डेटा फॉर्मेट है जो डेटा को संग्रहित और ट्रांसफर करने के लिए इस्तेमाल होता है। यह प्रोग्रामर्स को सरल, पठनीय और हल्के तरीके से डेटा का आदान-प्रदान करने में मदद करता है। 

## How to: (कैसे करें:)
हम Java में JSON से काम करने के लिए `org.json` library का इस्तेमाल कर सकते हैं।

पहले, आपको `org.json` library को अपने project में जोड़ना होगा। यह Maven या Gradle के माध्यम से आसानी से किया जा सकता है।

### JSON Object Create and Read करना:
```java
import org.json.JSONObject;

public class JSONExample {
    public static void main(String[] args) {
        // JSON Object create करना
        JSONObject obj = new JSONObject();
        obj.put("name", "Raj");
        obj.put("age", 30);
        obj.put("isMarried", false);

        // JSON Object से data read करना
        System.out.println("Name: " + obj.getString("name"));
        System.out.println("Age: " + obj.getInt("age"));
        System.out.println("Is Married: " + obj.getBoolean("isMarried"));
    }
}
```

### Sample Output:
```plaintext
Name: Raj
Age: 30
Is Married: false
```

## Deep Dive (गहराई से विचार):
JSON 2001 में डगलस क्रॉकफोर्ड द्वारा परिचय किया गया। XML के विकल्प के रूप में इसे डेटा फॉर्मेट के लिए सराहा गया क्योंकि यह अधिक संक्षेप और त्वरित था। Java में JSON पार्स करने के लिए कई लायब्रेरीज हैं जैसे कि `Gson`, `Jackson`, और `org.json`. चुनाव किसी भी प्रोजेक्ट के आवश्यकताओं और प्रोग्रामर की पसंद पर निर्भर करता है।

`org.json` library में `JSONObject`, `JSONArray` जैसे क्लासेज हैं जो प्रोग्रामर को JSON ऑब्जेक्ट्स और ऐरेज से काम करने में सुविधाजनक बनाते हैं। जावा के Object-oriented features के साथ मिलकर JSON handling बहुत ही सरल और प्रभावी बन जाता है।

## See Also (और भी देखें):
- JSON परिचय और गाइड्स: [W3Schools JSON Tutorial](https://www.w3schools.com/js/js_json_intro.asp)
- `org.json` library documentation: [GitHub org.json](https://github.com/stleary/JSON-java)
- JSON के साथ अन्य प्रसिद्ध लायब्रेरीज: [Gson GitHub](https://github.com/google/gson), [Jackson GitHub](https://github.com/FasterXML/jackson)