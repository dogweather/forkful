---
title:                "Go: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

YAML तार की कार्यवाही में शामिल होने का कारण है कि यह एक लोकप्रिय फॉर्मेट है जो डेटा संरचनाओं को सरलता से संगठित करता है। यह प्रोग्रामिंग डेवलपरों को एक अधिक सुविधाजनक और स्क्रिप्टिंग भाषा प्रदान करता है जो उन्हें उनके टेक्नोलॉजी स्टैक को संचालित करने में मदद करता है।

## कैसे करें

```Go
package main

import (
    "fmt"
    "log"

    "gopkg.in/yaml.v2"
)

type Movie struct {
    Name  string
    Year  int  `yaml:"released"`
    Color bool `yaml:"color,omitempty"`
    Actors []string
}

func main() {
    movie := Movie{
        Name:  "Inception",
        Year:  2010,
        Color: true,
        Actors: []string{"Leonardo DiCaprio", "Joseph Gordon-Levitt", "Ellen Page", "Tom Hardy"},
    }

    movieYaml, err := yaml.Marshal(movie)
    if err != nil {
        log.Fatalf("error: %v", err)
    }
    fmt.Println(string(movieYaml))
}
```

उपरोक्त कोड उदाहरण में, हमने YAML आगमन संरचना का उपयोग करके एक फिल्म की जानकारी को YAML फॉर्मेट में एक स्ट्रिंग में रूपांतरित किया है। आपको पूर्ववेत्ता पैकेज को इन्स्टॉल करने की आवश्यकता हो सकती है। अतिरिक्त साधनों के साथ साथ, यह प्रत्येक गो प्रोग्रामिंग आगमन से संबंधित है।

## गहराई धाव

YAML कस्टमाइजेशन की अनेक मुख्यताएं हैं जो इसे ईंधनतरूपी बनाती हैं, जिससे की आप अपने प्रोजेक्ट पर उल्लेखनीय प्रतिक्रिया मोडल को आसानी से विकसित कर सकें। यांग कोडिंग के समय, इसे जटिल वास्तविक जगह देने का प्रयास करना थोड़ा सक्रियदुपर्यास बनाता है, क्योंकि आप किशोरों के साथ खेलकर निखारते हैं आपकी प्रोजेक