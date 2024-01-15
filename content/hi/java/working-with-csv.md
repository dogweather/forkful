---
title:                "कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना।"
html_title:           "Java: कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना।"
simple_title:         "कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना।"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों

दैनिक उपयोग में, हम कई बार डेटा को स्प्रेडशीट और डेटा बेस की फ़ॉर्मेट में लाभान्वित करते हैं। और वह फ़ॉर्मेट है CSV या "Comma Separated Values". CSV को पढ़ने और लिखने के लिए आसानता के कारण, हम जावा में इसके साथ काम करने आसान होता है।

## कैसे करें

जावा में CSV के साथ काम करने का सबसे आसान तरीका है स्ट्रिंग पाइपलाइनर का उपयोग करना। यह उदाहरण निम्न रूप में हो सकता है।

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class CSVReader {

    public static void main(String[] args) {

        // CSV फ़ाइल को पढ़ें
        String csvFile = "employees.csv";
        String line = "";
        String csvSplitBy = ",";

        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {

            // प्रत्येक पंक्ति को अलग करें और उसे छोड़ें
            while ((line = br.readLine()) != null) {

                String[] employee = line.split(csvSplitBy);

                // हर स्ट्रिंग को पढ़ें और उचित परिवर्तन करें
                String name = employee[0];
                int age = Integer.parseInt(employee[1]);
                String position = employee[2];

                // प्रिंट करें सूची को
                System.out.println("Name: " + name + ", Age: " + age + ", Position: " + position);

            }

        } catch (IOException e) {
            e.printStackTrace();
        }

    }
}
```

आप भी अपनी कस्टम कोडिंग लॉजिक जोड़ सकते हैं। यह उपरोक्त उदाहरण में सभी पंक्तियां ऊपरी तरफ से सही स्थानों पर हैं और वे कभी-कभी सही विधि में नहीं होंगे। इसलिए, आपको कोड में उत्समता और सावधानी से काम करने हो सकता है।

## गहरी तकनीक

CSV परिसंबंधित सुझाव:
- सुनिश्चित करें कि सभी आदेशों के साथ सही विशेषताओं का सामना किया जा रहा है
- सुनिश्चित करें कि स्पेशल अक्षर ' और " सामान्य पठन-लिखन से भिन्न