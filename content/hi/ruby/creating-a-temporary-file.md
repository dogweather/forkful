---
title:                "अस्थायी फ़ाइल बनाना"
html_title:           "Ruby: अस्थायी फ़ाइल बनाना"
simple_title:         "अस्थायी फ़ाइल बनाना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या है और क्यों: 
एक अस्थायी फ़ाइल को बनाना, उपेक्षित फ़ाइल होता है जो केवल ऐसे समय के लिए उपलब्ध रहता है जब आपको प्रोग्राम के दौरान कुछ सामान्य डेटा को अस्थाई रूप से स्थानांतरित करने की ज़रूरत होती है। प्रोग्रामर्स अक्सर अस्थायी फ़ाइल संचालन का उपयोग करते हैं, जैसे कि डेटा बेस डेटाबेस सुविधाओं के लिए।

## कैसे करें: 
आप अपने रेली प्रोग्राम में अस्थायी फ़ाइलों को बनाने के लिए आसानी से उपयोग कर सकते हैं। निम्न उदाहरण से आप अपने प्रोग्राम में अस्थायी फ़ाइलों को कैसे बना सकते हैं देख सकते हैं:

उदाहरण 1: एक रिक्त अस्थायी फ़ाइल बनाएँ
```Ruby
file = Tempfile.new
puts "Temporary file created at #{file.path}"
```

उदाहरण 2: फाइल में डेटा लिखें
```Ruby
file = Tempfile.new
file.write("Hello world!")
file.rewind
puts "Data written to temporary file:\n#{file.read}"
```

उदाहरण 3: फाइल में से डेटा पढ़ें
```Ruby
file = Tempfile.new
file.write("Hello world!")
file.rewind
puts "Reading data from temporary file:\n#{file.read}"
```

## गहराई तक पदार्थ : 
अस्थायी फ़ाइलों को आमतौर पर कोड का एक हिस्सा ही समझा जाता है, लेकिन अस्थायी फ़ाइल का इतिहास रिक बनाने वाले प्रोग्रामिंग भाषाओं के साथ संबंधित है। अस्थायी फ़ाइलों के प्रयोग के अलावा, आप फ़ाइल को स्थाई रूप से भी स्थानांतरित कर सकते हैं और अस्थायी फोल्डरों का उपयोग कर सकते हैं। इसके अलावा अस्थायी फ़ाइलों की कुछ अवधारणाओं में भी फ़ायदे हैं जो आपको आपके प्रोग्राम के विकास को आसान बना सकते हैं।

## अन्य जानकारी: 
अस्थायी फ़ाइल बनाने के बारे में अधिक जानकारी के लिए आप निम्नलिखित स्रोतों की जांच कर सकते हैं:
- [Ruby डॉक्यूमेंटेशन: Tempfile कक्ष](https://ruby-doc.org/stdlib-2.7.2/libdoc/tempfile/rdoc/Tempfile.html)
- [Ruby सीखने के कॉर्स: अस्थायी फ़ाइल](https://www.ruby-lang.org/en/documentation/quickstart/2/)
- [GeeksforGeeks: रेली में अस्थायी फ़ाइल और अस्थाई फ़ोल्डर](https://www.geeksforgeeks.org/temporary-files-folders-ruby/)
- [Upcase: रेली में अस्थायी फ़ाइल कैसे बनाएं](https://thoughtbot.com/upcase/videos/temporary-file-primer)