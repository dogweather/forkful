---
title:                "Kotlin: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 방법에 대해 알고 싶은 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 데이터를 분석하고 처리하기 위해 텍스트 파일을 읽어야 할 수도 있고, 프로그램을 디버깅하기 위해 로그 파일을 읽어야 할 수도 있습니다. 어떤 이유든지, 텍스트 파일을 읽는 방법은 프로그래밍에서 매우 유용하고 중요한 기술입니다.

## 방법

텍스트 파일을 읽는 가장 기본적인 방법은 `BufferedReader` 클래스를 사용하는 것입니다. 아래는 Kotlin 코드로 텍스트 파일을 읽는 예시입니다.  

```Kotlin
import java.io.BufferedReader
import java.io.File
import java.io.FileReader
import java.io.FileNotFoundException

fun main() {
    // 읽을 파일 경로
    val filePath = "sample.txt"

    // 파일이 존재하는지 확인
    val file = File(filePath)
    if (!file.exists()) {
        println("파일이 존재하지 않습니다.")
        return
    }

    // 파일을 읽기 위한 BufferedReader 생성
    val reader = BufferedReader(FileReader(filePath))
    // 한 줄씩 읽어오기
    var line = reader.readLine()
    while (line != null) {
        println(line)
        line = reader.readLine()
    }

    // 리소스 반환
    reader.close()
}
```

위 코드를 실행하면, `sample.txt` 파일의 내용이 한 줄씩 출력됩니다. 만약 파일을 찾을 수 없는 경우, `FileNotFoundException`이 발생합니다.

## 자세히 알아보기

텍스트 파일을 읽는 방법에 대해 이해하려면, 몇 가지 중요한 점을 알아야 합니다.

* `BufferedReader` 클래스는 `readLine()` 메소드를 통해 파일의 한 줄씩 읽어올 수 있습니다.
* 파일을 열고 읽은 후, `close()` 메소드를 통해 리소스를 반환해야 합니다. 이는 파일의 일부를 읽어오는 것이 아니라 파일을 전체적으로 읽어오기 때문입니다.

따라서, 텍스트 파일을 읽는 코드를 작성할 때에는 이러한 포인트를 유의해야 합니다.

## 더 알아보기

위에서는 가장 기본적인 방법으로 텍스트 파일을 읽는 방법을 살펴보았지만, Kotlin에서는 다양한 방법으로 텍스트 파일을 읽을 수 있습니다. 예를 들어, Java의 `Scanner` 클래스를 Kotlin에서도 사용할 수 있습니다. 또한, 메모리를 더 적게 사용하는 `BufferedInputStream` 클래스를 사용할 수도 있습니다. 여러분에게 맞는 방법을 선택하여 텍스트 파일을 읽어보세요.

## 참고 자료

* [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/basic-input-output.html)
* [GeeksforGeeks - Kotlin | Read text file](https://www.geeksforgeeks.org/kotlin-read-text-file/)