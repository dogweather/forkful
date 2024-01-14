---
title:                "Kotlin: 임시 파일 만들기"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜 만들까요?

임시 파일을 생성하는 이유는 다양합니다. 먼저, 프로그래밍을 하다 보면 임시 파일이 필요한 순간이 자주 생기기 때문에 필수적인 작업이 됩니다. 또한, 임시 파일은 프로그램에서 사용자의 개인적인 정보를 저장할 때 유용하게 사용될 수 있습니다. 그리고 임시 파일은 중요한 파일이 아닌 일시적인 데이터를 저장할 때 좋은 방법이 될 수 있습니다. 

# 만드는 방법은?

코틀린에서 임시 파일을 만드는 방법은 간단합니다. 먼저, 주어진 파일 이름과 확장자를 가진 임시 파일을 만들고 싶은 경우에는 `createTempFile()` 함수를 사용하면 됩니다. 다음은 이 함수를 이용하여 임시 파일을 생성하는 예시 코드입니다.

```Kotlin
// 파일 이름과 확장자를 지정하여 임시 파일 생성
val tempFile = createTempFile("temp", ".txt")

// 임시 파일의 경로 출력
println(tempFile.absolutePath)

// 임시 파일 삭제
tempFile.delete()
```

위 코드의 실행 결과는 다음과 같습니다.

```console
C:/Users/User/AppData/local/temp/temp282875078667436377.txt
```

코드를 실행할 때마다 임시 파일의 이름이 달라질 수 있습니다. 이를 방지하기 위해 `createTempFile()` 함수 대신 `createTempFile(prefix: String, suffix: String, directory: File)` 함수를 사용하여 임시 파일을 생성할 수 있습니다. 이 함수는 파일 이름에 접두사와 접미사를 지정할 수 있고, 원하는 디렉토리에 임시 파일을 생성할 수 있도록 지정할 수 있습니다.

아래는 `createTempFile()` 함수를 사용하여 이름과 확장자를 지정하지 않고 임시 파일을 생성하는 예시 코드입니다.

```Kotlin
// 임시 파일 생성
val tempFile = createTempFile()

// 임시 파일의 경로 출력
println(tempFile.absolutePath)

// 임시 파일 삭제
tempFile.delete()
```

위 코드의 실행 결과는 다음과 같습니다.

```console
C:/Users/User/AppData/local/temp/tmp4793270537837907704.tmp
```

# 딥다이브

임시 파일을 생성하는 더 많은 옵션과 기능을 알고 싶다면 `createTempFile` 함수의 문서를 참고해야 합니다. 이 함수는 여러 매개변수를 받을 수 있고, 자세한 설명과 예시를 함께 제공합니다. 

아래는 `createTempFile` 함수의 매개변수 목록입니다.

- `prefix`: 임시 파일 이름의 접두사를 지정합니다. 기본값은 `tmp`입니다.
- `suffix`: 임시 파일 이름의 접미사를 지정합니다. 기본값은 `.tmp`입니다.
- `directory`: 임시 파일을 생성할 디렉토리를 지정합니다. 기본값은 `java.io.tmpdir` 시스템 속성에 의해 결정됩니다.
- `attributes`: 생성된 임시 파일의 속성을 지정합니다. 속성은 `arrayOf()` 함수를 이용하여 여러 개 지정할 수 있습니다.

# 더 알아보기

다른 코틀린 프로그래밍에 유용한 팁과 기능을 알고 싶다면 아래 링크를 참고해 보세요.

## 더 알아보기

- [코틀린 공식 문서](https://kotlinlang.org/docs/home.html)
- [코틀린 알고리즘 문제 풀이](https://www.hyperskill.org/learn/step/3871)
- [코틀린 고급 개발자