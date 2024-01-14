---
title:                "Kotlin: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 만드는 것에 관심이 있는 사람들에게는 일상적인 일이다. 파일에 정보를 저장하고 정리할 수 있기 때문에 개발자들에게 유용하다. 

## 만드는 법

Kotlin에서 텍스트 파일을 만드는 것은 매우 쉽다. 먼저, ```FileWriter``` 클래스를 사용하여 새로운 파일을 만든다. 그런 다음 생성한 파일을 이용하여 데이터를 입력하고 저장한다. 아래의 예제를 참고하자.

```Kotlin
import java.io.File
import java.io.FileWriter

fun main() {
    val fileName = "newFile.txt"
    val file = FileWriter(fileName)

    file.write("안녕하세요! 이것은 텍스트 파일입니다.")
    file.close()
    
    println("$fileName 파일이 성공적으로 만들어졌습니다.")
}
```

위의 코드를 실행하면 현재 작업 디렉토리에 "newFile.txt"라는 이름의 파일이 생성될 것이다. 파일을 열어보면 "안녕하세요! 이것은 텍스트 파일입니다."라는 내용이 포함되어 있는 것을 확인할 수 있다.

## 심화 공부

텍스트 파일을 만드는 방법 뿐만 아니라 파일에 추가적인 데이터를 입력하는 방법도 알아보자.

```Kotlin
import java.io.File
import java.io.FileWriter

fun main() {
    val fileName = "existingFile.txt"
    val file = FileWriter(fileName, true)

    file.append("여기에 추가할 내용을 입력합니다.")
    file.close()
    
    println("$fileName 파일에 내용이 성공적으로 추가되었습니다.")
}
```

위의 코드에서는 ```FileWriter``` 생성자의 두 번째 매개변수로 ```true``` 값을 넘겨줌으로써 기존 파일에 내용을 추가할 수 있다. 이를 통해 파일에 여러 번의 입력을 할 수 있는 여러 가지 방법을 시도해보길 바란다.

## 더 알아보기

Kotlin 공식 문서에서 파일 관련 클래스들에 대해 더 자세하게 알아볼 수 있다. [자바 클래스와의 상호 운용성](https://kotlinlang.org/docs/interop.html)에 관한 글도 참고하면 좋다.

## 관련 링크

- [Kotlin 공식 문서: 파일 관련 클래스](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/) 
- [자바 클래스와의 상호 운용성](https://kotlinlang.org/docs/interop.html)
- [Kotlin에서 파일 다루기](https://velog.io/@bsmoon2100/Kotlin%EC%9D%98-%ED%8C%8C%EC%9D%BC-%EB%8B%A4%EB%A3%A8%EA%B8%B0)