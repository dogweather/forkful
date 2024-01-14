---
title:                "Kotlin: 텍스트 파일 읽기"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜
텍스트 파일을 읽는 것이 왜 중요한지 궁금하신가요? 이 블로그 포스트에서는 Kotlin으로 쉽게 텍스트 파일을 읽는 방법을 알려드립니다.

## 방법
텍스트 파일을 읽는 것은 Kotlin에서 간단한 작업입니다. 적절한 라이브러리를 사용하여 파일을 열고, 데이터를 읽고, 필요한 처리를 하면 됩니다. 아래의 예제를 참고해보세요.

```Kotlin
val file = File("mytextfile.txt")
val bufferedReader: BufferedReader = file.bufferedReader()
val inputString = bufferedReader.use { it.readText() }
println(inputString)
```

위 예제에서는 코틀린의 `File` 클래스와 `bufferedReader()` 함수를 사용하여 파일을 열고, `use()` 함수를 사용하여 데이터를 읽었습니다. 마지막으로, `println()` 함수를 사용하여 읽은 데이터를 콘솔에 출력했습니다.

만약 파일의 내용을 한 줄씩 읽고 싶다면 다음과 같이 코드를 수정할 수 있습니다.

```Kotlin
val file = File("mytextfile.txt")
file.forEachLine {
    println(it)
}
```

위 예제에서는 파일의 각 줄을 읽어 `forEachLine()` 함수를 사용하여 출력하였습니다.

## 더 깊이드러가기
텍스트 파일을 읽는 과정에서 발생할 수 있는 에러나 예외 상황에 대비해야 합니다. 다음 예제는 `try-catch` 구문을 사용하여 예외 상황을 처리하는 방법을 보여줍니다.

```Kotlin
val file = File("mytextfile.txt")
try {
    val bufferedReader: BufferedReader = file.bufferedReader()
    val inputString = bufferedReader.use { it.readText() }
    println(inputString)
} catch (e: FileNotFoundException) {
    println("파일을 찾을 수 없습니다.")
} catch (e: IOException) {
    println("파일을 읽는 도중 에러가 발생했습니다.")
}
```

또한 파일을 읽을 때 인코딩에 주의해야 합니다. 파일의 인코딩 정보가 맞지 않으면 읽은 데이터가 손상될 수 있습니다. 따라서 `charset` 매개변수를 사용하여 인코딩 방식을 명시하는 것이 좋습니다.

```Kotlin
val file = File("mytextfile.txt")
val bufferedReader: BufferedReader = file.bufferedReader(Charsets.UTF_8)
val inputString = bufferedReader.use { it.readText() }
println(inputString)
```

## 더 알아보기
만약 더 자세히 알고 싶다면 다음 링크를 참고해보세요.

[코틀린 공식 문서](https://kotlinlang.org/docs/working-with-files.html)

## 관련 링크
- [자바 파일 입출력 강좌](https://www.youtube.com/watch?v=vwJQg5bbCoQ)
- [Kotlin으로 텍스트 파일 샘플 읽기](https://stackoverflow.com/a/39934227)
- [Kotlin 파일 입출력 예제](https://gist.github.com/DaRkG0D/925567)