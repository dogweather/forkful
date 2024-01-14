---
title:    "Kotlin: 새 프로젝트 시작하기"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜 시작하는가?

새로운 프로젝트를 시작하는 것은 어떠한 이유로 인해 중요합니다. 시간과 노력을 들여 새로운 프로젝트를 시작하는 것은 도전적이고 재미있는 일이지만, 그만큼 보상도 커질 수 있습니다. 새로운 스킬을 배우고 다양한 아이디어를 실현할 수 있는 기회가 되기 때문에 새로운 프로젝트를 시작하는 것은 매우 중요합니다.

## 방법

Kotlin으로 새로운 프로젝트를 시작하는 방법을 살펴보겠습니다. 먼저 아래의 코드 블럭을 사용하여 프로젝트를 만듭니다.

```Kotlin
fun main() {
    println("Hello, world!")
}
```

위의 코드 블럭은 새로운 프로젝트를 시작하는 기본적인 코드입니다. 코드를 실행하면 "Hello, world!"라는 메시지를 출력할 수 있습니다. 이제 여러분은 이 코드를 기반으로 새로운 기능을 추가할 수 있습니다. 예를 들어, 아래의 코드는 사용자가 입력한 이름을 화면에 출력하는 코드입니다.

```Kotlin
fun main() {
    println("What is your name?")
    val name = readLine() //사용자로부터 입력받은 값 저장
    println("Hello, $name!") //입력받은 이름 출력
}
```

새로운 기능을 추가하는 것 외에도, Kotlin을 사용하여 데이터를 처리하거나 다른 시스템과 연동하는 등 다양한 작업을 할 수 있습니다.

## 더 깊게 들어가기

새로운 프로젝트를 시작하는 것은 단순히 코드를 작성하는 것 이상의 의미를 가집니다. 이를 통해 여러분은 프로그래밍 지식을 쌓고 문제를 해결하는 능력을 향상할 수 있습니다. 또한 여러분의 창의성을 발휘하여 다양한 아이디어를 생각해내고 구현할 수 있습니다. 새로운 프로젝트를 시작한다는 것은 자신을 도전하고 개발하는 데 큰 보람을 주는 일입니다.

## 같이 보기

- [Kotlin 공식 사이트](https://kotlinlang.org/)
- [Kotlin 개발 환경 구축하기](https://blog.naver.com/naverdev/221579424122)
- [Kotlin 강의 (네이버 부스트코스)](https://www.edwith.org/boostcourse-android)