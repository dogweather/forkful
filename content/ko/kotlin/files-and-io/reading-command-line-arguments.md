---
date: 2024-01-20 17:56:46.061732-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB098\uC694?) \uAC04\uB2E8\
  \uD788, `Array<String>` \uD0C0\uC785\uC758 `args`\uB97C \uBA54\uC778 \uD568\uC218\
  \uB85C \uBC1B\uC544 \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uCF54\uB4DC \uC608\uC2DC\uC640\
  \ \uACB0\uACFC\uB97C \uBCF4\uC5EC\uB4DC\uB9AC\uC8E0."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.930905-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB098\uC694?) \uAC04\uB2E8\uD788\
  , `Array<String>` \uD0C0\uC785\uC758 `args`\uB97C \uBA54\uC778 \uD568\uC218\uB85C\
  \ \uBC1B\uC544 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

## How to: (어떻게 사용하나요?)
간단히, `Array<String>` 타입의 `args`를 메인 함수로 받아 사용합니다. 코드 예시와 결과를 보여드리죠.

```Kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("첫 번째 인자: ${args[0]}")
    } else {
        println("인자가 제공되지 않았습니다.")
    }
}
```

커맨드 라인에서 다음과 같이 실행하세요:
```
kotlinc Main.kt -include-runtime -d Main.jar
java -jar Main.jar 첫번째인자
```

출력 예시:
```
첫 번째 인자: 첫번째인자
```

## Deep Dive (심층 분석)
커맨드 라인 인수 읽기는 오래된 개념이며 Unix와 같은 초기 운영 체제에서부터 사용되어 왔습니다. Kotlin에서는 그 흔적을 따르고 있죠. `args` 배열을 통해 접근하는 것이 기본 방식이지만, 라이브러리를 사용하면 더 편리하게 인수를 파싱하고 사용할 수 있습니다. 예를 들어, `kotlinx-cli` 라는 라이브러리는 선언적 DSL을 통해 인수를 정의하고 처리하는 것을 도와줍니다.

Kotlin은 자바 플랫폼 위에서 동작하므로, `args` 배열을 처리하는 방식도 사실상 Java와 동일합니다. 그럼에도 Kotlin은 코틀린스러운 문법을 통해 더 풍부하고 간결한 표현이 가능하게 해줍니다. 람다 표현식이나 확장 함수 같은 기능을 통해, 커맨드 라인 인수를 더욱 쉽고 강력하게 다룰 수 있습니다.

## See Also (더 보기)
- Kotlin 공식 문서: [Kotlin Args](https://kotlinlang.org/docs/faq.html#how-do-i-pass-parameters-to-the-main-function)
- 커맨드 라인 인수를 위한 Kotlinx-cli 라이브러리: [Kotlinx-cli GitHub](https://github.com/Kotlin/kotlinx-cli)
