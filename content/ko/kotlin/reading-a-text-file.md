---
title:                "텍스트 파일 읽기"
html_title:           "Kotlin: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜 읽어야 하는가?

텍스트 파일을 읽는 것은 프로그래밍에서 필수적입니다. 이를 통해 데이터를 처리하고 분석하며, 필요한 정보를 추출할 수 있습니다.

## 방법

```Kotlin
fun main() {
  val file = File("textfile.txt")
  val lines = file.readLines()
  for (line in lines) {
    println(line)
  }
}
```

위의 코드 예시는 Kotlin에서 텍스트 파일을 읽는 가장 기본적인 방법입니다. `File` 클래스를 사용하여 파일을 읽은 후, `readLines()` 함수를 이용해 각 줄을 `lines` 변수에 저장합니다. 그리고 `for` 반복문을 통해 각 줄을 출력합니다.

### 파일 경로 지정하기

만약 텍스트 파일이 다른 경로에 있다면, `File()` 생성자에 파일 경로를 지정해주어야 합니다. 예를 들어, `File("/Users/username/Desktop/textfile.txt")`와 같은 방식으로 파일 경로를 지정할 수 있습니다.

### 파일 내용 읽기

파일 전체를 한 번에 읽어오는 것이 아니라, `BufferedReader` 클래스를 사용하여 한 줄씩 파일을 읽을 수도 있습니다.

```Kotlin
fun main() {
  val file = File("textfile.txt")
  val reader = BufferedReader(FileReader(file))
  var line: String?
  line = reader.readLine()
  while (line != null) {
    println(line)
  }
}
```

위의 코드는 `BufferedReader`를 이용하여 한 줄씩 파일을 읽어오는 예시입니다. `readLine()` 함수를 이용하여 한 줄을 읽어온 후, `while` 반복문을 통해 파일의 끝까지 한 줄씩 출력합니다.

## 깊이 들어가기

텍스트 파일을 읽는 것은 매우 중요한 과정입니다. 다양한 방식으로 파일을 읽을 수 있으며, 이를 통해 응용 프로그램에서 파일 데이터를 효율적으로 처리할 수 있습니다.

### 예외처리하기

텍스트 파일을 읽을 때, 파일이 존재하지 않거나 권한이 없는 경우 등 예외 상황이 발생할 수 있습니다. 따라서 예외처리를 통해 프로그램이 정상적으로 동작할 수 있도록 해야 합니다. `try-catch` 구문을 사용하여 예외를 처리할 수 있습니다.

```Kotlin
fun main() {
  try {
    val file = File("textfile.txt")
    val lines = file.readLines()
    for (line in lines) {
      println(line)
    }
  } catch (e: FileNotFoundException) {
    println("파일을 찾을 수 없습니다.")
  } catch (e: SecurityException) {
    println("파일에 대한 권한이 없습니다.")
  }
}
```

위의 예시는 `FileNotFoundException`과 `SecurityException`을 처리하는 방법을 보여줍니다. 여러 가지 예외 상황에 대비하여 적절한 예외처리를 반드시 해주어야 합니다.

## 참고 자료

- [Kotlin 공식 문서 - 파일 입출력](https://kotlinlang.org/docs/tutorials/kotlin-for-py/files.html)
- [basic I/O in Kotlin](https://www.baeldung.com/kotlin-io)
- [Kotlin Tutorial - 파일 입출력](https://www.tutorialspoint.com/kotlin/kotlin_file_io.htm)

## 관련 자료

- [Kotlin으로 파일 쓰기](https://www.gnu-linux.net/kotlin/write-a-file.html)
- [Kotlin Text File Processing 예제](https://www.journaldev.com/17061/kotlin-text-file-processing-append-example)