---
title:                "Kotlin: अगर एक निर्देशिका मौजूद है, तो कैसे जांचें?"
simple_title:         "अगर एक निर्देशिका मौजूद है, तो कैसे जांचें?"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Kyun

Kisi bhi vyakti ko directory ke maudrikshan ke baare mein jaanana bahut mahatvapurna hai, khas kar jab ve koi naya karyakram likh rahe hote hain. Directory ka maujood hona bahut hi mahatvapurna hota hai kyonki iske na hone par, karyakram jari rakhane se pahale vaah, error ka samaana kar sakta hai. Isliye directory ke maujood hona ka jaanana bahut hi mahatvapurna hai.

## Kaise Kare

```Kotlin
import java.io.File

fun main(args: Array<String>) {
    val directory = File("Example/Directory")
    if(directory.isDirectory){
        println("Directory is present.")
    }else{
        println("Directory does not exist.")
    }
}
```

`isDirectory` prakriya, directory mein chal rahi spardha ke karyakram mein bahut hi mahatvapurna hai. Yah upayokta ko goshana karne ki anumati deta hai ki directory maujood hai ya nahin. Ismein, `File` jaruri hota hai jiske madad se karyakram directory ke path ko shaamil kar deta hai. Iske baad, `isDirectory` ka upayog karyakram mein kiya jaata hai jisse pata chalta hai ki directory maujood hai ya nahin. Agar directory maujood hai, toh "Directory is present." ka output prastaavit karta hai. Lekin agar directory maujood nahin hai, toh "Directory does not exist." ka output prastaavit karta hai.

## Gahraai Mein Jaayein

Directory ke maujood hona ka jaanana jindagi bhar kaam mein upyogi hota hai, khaaskar jab aap kisi naye karyakram ko likh rahe hote hain. Is prakriya mein, `File` ka upayog karyakram ke liye directory ke path ko bhejne ke liye kiya jaata hai. Phir, `isDirectory` ka upayog directory ke maujood hona ka pata lagane ke liye kiya jaata hai. Is tarike se, directory ke maujood na hona se judee saaree samasyaen is prakriya se door ho jaati hain.

## Dekhein Bhi

- [Java Tutorial: Checking if a Directory Exists](https://www.tutorialspoint.com/java/io/file_exists.htm)
- [Kotlin Tutorial: File Handling](https://www.javatpoint.com/kotlin-file-handling)
- [GeeksforGeeks: Checking if a Directory Exists in Kotlin](https://www.geeksforgeeks.org/checking-existence-of-a-directory-in-kotlin/)