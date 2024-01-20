---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

새로운 프로젝트를 시작한다는 것은 코딩을 통해 새로운 소프트웨어나 기능을 만드는 첫 단계입니다. 이를 통해 프로그래머는 사용자에게 가치를 제공하거나 개인 능력을 향상시키기 위해 필요한 부분을 구현합니다.

## 방법:

새로운 Kotlin 프로젝트를 생성하는 방법을 보여주겠습니다.
```Kotlin
// IntelliJ에서 새 Kotlin 프로젝트를 만드는 방법
// 1) File > New > Project 클릭
// 2) Kotlin > Kotlin/JVM 선택
// 3) 프로젝트의 이름과 위치를 설정, 나머지는 기본값으로 남겨둠
// 4) Finish 클릭
```
이것은 단순한 "Hello World" 프로그램을 출력하는 코틀린 프로그램입니다.
```Kotlin
fun main(args: Array<String>) {
    println("Hello, World!")
}
```
이 프로그램을 실행하면 콘솔창에 "Hello, World!" 출력됩니다.

## Deep Dive:

프로젝트를 새로 시작하는 것은 프로그래밍의 기본적인 특성입니다. 자바언어 기반으로 만들어진 Kotlin은 새로운 프로젝트에 대한 접근부터 JVM과 호환성까지 고려되었습니다. 시작하기에 대한 다른 방법으로는 직접 코드를 작성하거나, 템플릿을 사용하는 방법이 있습니다. 프로젝트의 초기 설정은 기본 디렉토리 구조, 프로젝트 설정, 빌드 설정 등을 포함하며, 이는 프로젝트의 규모와 복잡성에 따라 달라집니다.

## 참고:

- Kotlin 공식 문서: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- InnovationM blog의 Kotlin 프로젝트 시작 방법 가이드: [https://www.innovationm.com/creating-kotlin-project-in-intellij-idea/](https://www.innovationm.com/creating-kotlin-project-in-intellij-idea/)
- Stack Overflow에서 Kotlin 관련 질문 참고: [https://stackoverflow.com/questions/tagged/kotlin](https://stackoverflow.com/questions/tagged/kotlin)