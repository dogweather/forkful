---
title:                "Java: टेक्स्ट फाइल को पढ़ना"
simple_title:         "टेक्स्ट फाइल को पढ़ना"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Kyon
Agar aap ek programmer hai aur Java ka istemal karte hai, to aapko text file padhna sikhna zaroori hai. Text file padhna, jab koi badi ya complex file ko access karna ho, bahut useful hota hai. Isse aap asaani se data ko manage aur manipulate kar sakte hai.

## Kaise Kare
```Java
import java.io.File;
import java.util.Scanner;

public class TextFileReader {

    public static void main(String[] args) {

        try {
            // File object with the file path
            File file = new File("C:\\Users\\User\\Desktop\\mytextfile.txt");

            // Scanner object to read the file
            Scanner scanner = new Scanner(file);

            // Loop through each line of the file
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                System.out.println(line); // Print each line
            }
            scanner.close(); // Close the scanner object
        } catch (Exception e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }
}
```

Jaise upar diye gaye code mein dikhaya gaya hai, pehle hum File class ke object se file ka path set karte hai. Iske baad Scanner class ka object banakar `nextLine()` function ka istemal karke hum file ko padhte hai aur har line ko print karte hai. Code ka output kuch is tarah hoga:

```
Hello World!
This is a text file.
I am learning how to read a text file using Java.
```

## Gehri Jhaank
Text file padhna ek kaafi simple task nahi hai balki isme kaafi sare intricacies hai. Java mein, hum file ko 2 tareeke se padh sakte hai - character-by-character aur line-by-line. Humne upar character-by-character tareeka dekha hai, lekin kai baar hume line-by-line padhna hota hai tab hum `nextLine()` function ka istemal karte hai.

Ek important point jo hume dhyan rakhna chahiye text file padhte waqt, ye hai ki hume ye janna hoga ki text file ka dhariyaan character kitne hai. Isse hume file ko kis tareeke se padhna hai, ye decide kar sakte hai. Additionally, hum file ko read-only, write-only ya read-write mode mein khol sakte hai.

## Zyaada Jaane Ke Liye
Is blog post mein humne seekha ki kaise hum text file ko Java mein padh sakte hai. Aap is code ko modify kar sakte hai aur apne projects mein istemal kar sakte hai. Agar aapko aur jaankari chahiye text file aur Java ke baare mein, toh aap neeche diye gaye links ko check kar sakte hai:

* [Java File Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
* [Java Scanner Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
* [GeeksforGeeks - Reading Text File Using Java](https://www.geeksforgeeks.org/different-ways-reading-text-file-java/)
* [TutorialsPoint - Java Input/Output](https://www.tutorialspoint.com/java/java_files_io.htm)

## Dekhein
* [Urdu Blog Post about Reading a Text File in Java](https://github.com/salma-nyar/ByteHumans/blob/master/JBlogPosts/Java-ReadATextFile-Urdu.md)
* [Spanish Blog Post about File Handling in Java](https://github.com/juanpesp/ByteHumans/blob/master/JBlogPosts/Java-FileHandling-Spanish.md)