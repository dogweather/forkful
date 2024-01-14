---
title:                "Kotlin: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

왜 임시 파일을 생성하는 것에 참여해야 하는지 궁금하지 않으신가요? 임시 파일이란 메모리에 저장된 데이터를 디스크에 저장하기 위한 임시적인 파일을 의미합니다. 임시 파일은 우리의 작업에 도움이 되는 중요한 역할을 합니다.

## 방법

```Kotlin 
val tempFile = File.createTempFile("prefix", "suffix")
println("임시 파일의 경로: " + tempFile.absolutePath)
```

위 코드는 우리가 임시 파일을 생성하는 가장 간단한 방법입니다. `createTempFile` 함수는 `prefix`와 `suffix`로 설정된 파일 이름을 갖는 임시 파일을 생성합니다. `prefix`는 파일 이름의 앞에 붙는 문자열이며 `suffix`는 파일 이름의 뒤에 붙는 문자열입니다. 그리고 `absolutePath`는 파일의 절대 경로를 반환합니다.

## 딥 다이브

임시 파일을 생성하는 과정은 약간 복잡합니다. 먼저, 우리는 `File.createTempFile` 함수를 호출하여 `File` 객체를 얻습니다. 그 다음, 우리는 `createTempFile` 함수를 호출할 때 우리가 원하는 파일 이름을 지정해야 합니다. 이는 우리가 원하는 모든 파일 이름을 사용할 수 있는 임시 파일을 생성하는 것을 가능하게 합니다. 

임시 파일은 우리의 작업에 도움을 주는데, 때로는 우리가 특정 작업을 수행하기 위해 반복적으로 임시 파일을 생성해야 할 수도 있습니다. 이런 경우, 우리는 `File.deleteOnExit` 함수를 사용하여 프로그램이 종료될 때 임시 파일을 자동으로 삭제할 수 있도록 할 수 있습니다.

## 더보기

- [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/)
- [나동빈님의 코틀린 강좌](https://ndb796.tistory.com/358)
- [백기선님의 코틀린 스프링 부트 강의](https://www.inflearn.com/course/%EC%BD%94%ED%8B%80%EB%A6%B0-%EC%8A%A4%ED%94%84%EB%A7%81%EB%B6%80%ED%8A%B8#description)