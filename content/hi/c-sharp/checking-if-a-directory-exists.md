---
title:                "यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
html_title:           "Arduino: यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
simple_title:         "यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Directory का पता लगाना है कि वह मौजूद है या नहीं। Programmers इसे करते हैं ताकि errors को रोका जा सके जब कोई file या data उस directory में save किया जा रहा हो।

## How to: (कैसे करें:)
C# में directory का पता लगाने के लिए `System.IO` namespace का `Directory.Exists` method का इस्तेमाल करते हैं। 

```C#
using System;
using System.IO;

class Program {
    static void Main() {
        string directoryPath = @"C:\examplepath";

        if (Directory.Exists(directoryPath)) {
            Console.WriteLine("Directory मौजूद है।");
        } else {
            Console.WriteLine("Directory मौजूद नहीं है।");
        }
    }
}
```

Sample Output:
- अगर directory मौजूद है: `Directory मौजूद है।`
- अगर directory मौजूद नहीं है: `Directory मौजूद नहीं है।`

## Deep Dive (गहराई में जानकारी)
`System.IO.Directory.Exists` method C# में लंबे समय से है और यह .NET Framework के साथ ही आया था। इस method का use करते हुए, programmers सीधे check कर सकते हैं कि कोई directory है भी या नहीं बिना किसी exception के। 

Alternatives की बात करें, तो कुछ programmers `DirectoryInfo` class का `Exists` property का भी उपयोग कर सकते हैं, लेकिन `Directory.Exists` सरल और सीधी विधि है।

Implementation details में, यह method न सिर्फ directory की मौजूदगी चेक करता है, बल्कि यह भी ध्यान देता है कि उस path तक access की permissions हैं या नहीं। यदि path तक पहुंचने में असमर्थ होता है तो यह `false` return करता है।

## See Also (और अधिक जानकारी के लिए)
- Microsoft's documentation on `Directory.Exists`: [Link](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- MSDN article on `DirectoryInfo.Exists` vs `Directory.Exists`: [Link](https://docs.microsoft.com/en-us/dotnet/api/system.io.directoryinfo.exists)
- Stack Overflow discussions on file I/O in C#: [Link](https://stackoverflow.com/questions/tagged/c%23+file-io)