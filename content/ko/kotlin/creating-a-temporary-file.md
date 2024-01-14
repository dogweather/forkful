---
title:    "Kotlin: 임시 파일 만들기."
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜
 일시적인 파일을 생성하는 것이 좋은 이유는 무엇일까요? 잠시 저장할 필요가 있는 데이터를 다루거나 프로그램 실행 중에 임시적으로 생성되는 파일을 다루기 위해서입니다.

## 만드는 방법
아래는 코틀린으로 임시 파일을 만드는 예제 코드입니다. 

```Kotlin
// 임시 파일 생성
val tempFile = File.createTempFile("temp", ".txt")

// 임시 파일 경로와 이름 출력
println("임시 파일 경로: ${tempFile.absolutePath}")

// 임시 파일 삭제
tempFile.delete()

// 존재하지 않는 임시 파일 경로와 이름 출력
println("삭제 후 임시 파일 경로: ${tempFile.absolutePath}")

```

위 코드를 실행하면 아래와 같은 출력이 나오게 됩니다.

```shell
임시 파일 경로: C:\Users\Username\AppData\Local\Temp\temp527315413008874838.txt
삭제 후 임시 파일 경로: C:\Users\Username\AppData\Local\Temp\temp527315413008874838.txt
```

## 깊이 파헤치기
임시 파일을 생성하는 방법은 다양하지만 대부분의 경우 `File.createTempFile()` 함수를 사용합니다. 이 함수는 임시 파일의 경로와 이름을 인자로 받아서 임시 파일 객체를 생성해주는데, 인자로 넘겨준 이름 뒤에 임시 파일의 고유 번호가 붙어서 파일 이름이 생성됩니다. 또한, 프로그램이 종료될 때 임시 파일도 자동으로 삭제됩니다.

하지만 임시 파일을 직접 생성하고 관리하는 방법도 있습니다. `File.createTempFile()` 함수를 사용하지 않고 `java.io.File()` 생성자를 이용해 파일 객체를 생성한 뒤, `tempFile.deleteOnExit()` 메소드를 호출해 프로그램이 종료될 때 임시 파일을 자동으로 삭제하도록 설정할 수 있습니다.

## 또 다른 자료
* [코틀린 공식 문서 - 파일 다루기](https://kotlinlang.org/docs/reference/collections-overview.html)
* [블로그 - 코틀린 프로그래밍의 기초](https://blog.naver.com/YourUsername/1234)
* [코틀린 마스터북 - 파일 다루기](https://www.yes24.com/Product/Goods/97748479?scode=032&OzSrank=1)
* [코틀린 코리아 커뮤니티](https://kotlin-korea.github.io/)

## 참고
이번 글에서는 코틀린에서 임시 파일을 생성하는 간단한 예제를 소개했습니다. 임시 파일을 생성하는 방법은 다양하며, 상황에 따라 적합한 방법을 선택하여 사용할 수 있습니다. 임시 파일을 사용할 때는 프로그램 실행 중에 임시 파일이 자동으로 삭제되는지 주의해야 하며, 필요에 따라 수동으로 삭제해야 하는 경우도 있습니다. 자세한 내용은 참고 자료를 확인하시기 바랍니다.