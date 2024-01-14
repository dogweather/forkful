---
title:    "Java: टेक्स्ट फाइल को पढ़ना"
keywords: ["Java"]
---

{{< edit_this_page >}}

वजह: एक टेक्स्ट फ़ाइल पढ़ने का *क्यों* कोई इंसान करने में रुचि रखेगा।

जवाब कैसे: "```Java
File file = new File("myFile.txt");
Scanner sc = new Scanner(file);

while(sc.hasNextLine()){
    String line = sc.nextLine();
    System.out.println(line);
}

sc.close();
 ```"
संदर्भ: टेक्स्ट फ़ाइल पढ़ने के बारे में गहराई से जानकारी।

डिप डाइव: टेक्स्ट फ़ाइलें पढ़ने से पहले, हमें इसकी पिछली स्थिति को ध्यान में रखना चाहिए। अगर यह एक मुख्य फ़ाइल है, तो हमें इसका JDK प्रकार भी जानना चाहिए। हम कई तरह के फ़ाइल वर्णन पाते हैं जो हमें पढ़ने के लिए अनुसरण करने की आवश्यकता होती है। पिछला विवरण एक व्यक्ति जो एक ब्लॉक को संशोधित करता है, उसकी पिछली स्थिति और उसकी ऑफसेट को ध्यान में रखने के लिए बहुत उपयोगी हो सकती है।

"```Java
// पढ़ने के लिए टेक्स्ट फ़ाइल के प्रकार
File file = new File("myFile.txt");
Scanner sc = new Scanner(file);

while(sc.hasNextLine()){
    String line = sc.nextLine();
    System.out.println(line);
}
// यदि इस फ़ाइल को संपादित किया जाता है, तो हमें इसकी पिछली स्थिति और उसका ऑफसेट भी ध्यान में रखना चाहिए

sc.close();
 ```"

देखें भी: 

- [जावा मध्ये फाईलला कसे प्रवाह कशाला करायचे](https://www.geeksforgeeks.org/different-ways-reading-text-file-java/)
- [जावा मध्ये टेक्स्ट फ़ाइल पढ़ने का प्रक्रिया और उसके उदाहरण](https://www.tutorialspoint.com/java/io/java_io_filereader.htm)
- [जावा मध्ये टेक्स्ट फ़ाइल लिंक और संदर्भ जाल](https://beginnersbook.com/2014/04/java-read-link-files-new-bufferedreadernew-inputstreamreaderfilereaderfile-path/)