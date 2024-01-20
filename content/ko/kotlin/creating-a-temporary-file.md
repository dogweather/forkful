---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

---

## 무엇이며 왜 사용하는가?

임시 파일 생성은 프로그램이 임시적인 데이터를 저장하거나, 공유할 수 있는 파일을 만드는 것입니다. 이는 메모리 부족을 방지하거나 복잡한 데이터 공유을 위해 일반적으로 사용됩니다.

## 사용 방법:

```Kotlin
import java.nio.file.Files

fun main() {
    val tempFile = Files.createTempFile("tempFile", ".tmp")
    println("Temp file: $tempFile")
}
```

위의 코드를 실행하면 임시 경로에 임시 파일이 생성됩니다. 예를 들면, 이런 결과를 볼 수 있습니다:

```
Temp file: /tmp/tempFile3958495036.tmp
```

## 깊이 있는 분석:

임시 파일은 일반적으로 디스크 공간을 활용하여 대량의 데이터를 처리할 때 사용되며, Unix 시스템에서는 `/tmp` 폴더에 저장되곤 했습니다. 대안으로 `java.io.File#createTempFile`를 사용할 수 있지만, Kotlin에서는 `java.nio.file.Files#createTempFile`가 권장됩니다. 이 함수는 파일을 삭제해야 하는 책임을 프로그래머로부터 해제해 주는 장점이 있습니다.

## 참조:

Kotlin 공식 문서에 `createTempFile` 함수에 대한 자세한 설명이 있습니다:
_https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.nio.file.-files/create-temp-file.html_

또한, `java.io.File#createTempFile` 대하여 더 알고 싶다면 다음 자바 문서를 참조하십시오:
_https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-_