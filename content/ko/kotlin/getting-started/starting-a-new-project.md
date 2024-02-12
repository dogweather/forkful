---
title:                "새 프로젝트 시작하기"
aliases:
- /ko/kotlin/starting-a-new-project.md
date:                  2024-01-20T18:03:52.149756-07:00
model:                 gpt-4-1106-preview
simple_title:         "새 프로젝트 시작하기"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
새 프로젝트 시작하기는 빈 캔버스에 그림을 그리는 것과 같다. 프로그래머들은 아이디어를 실현하고 문제를 해결하기 위해 새로운 프로젝트를 만든다.

## How to: (방법)
Kotlin 프로젝트를 시작하기 위해 IntelliJ IDEA를 사용하는 예제다.

1. IntelliJ IDEA를 열고 "Create New Project"를 선택한다.
2. Kotlin/JVM을 선택하고 Next 버튼을 클릭한다.
3. 프로젝트 이름과 위치를 설정하고 Finish 버튼을 클릭한다.
4. src 폴더에 새 Kotlin 파일을 만들고 아래 코드를 작성한다.

```Kotlin
fun main() {
    println("Hello, Kotlin World!")
}
```

5. 프로그램을 실행하면 콘솔에 다음이 출력된다:

```Kotlin
Hello, Kotlin World!
```

## Deep Dive (심층 분석)
새 프로젝트를 시작하는 것은 코틀린에서 간단하다. IntelliJ IDEA는 코틀린 개발에 최적화된 환경을 제공한다. 역사적으로, 자바와 같은 언어는 많은 설정이 필요했지만, 코틀린은 네이티브 지원을 받아 더 간편해졌다. 대안으로, 커맨드 라인을 사용하거나, 다른 IDE를 사용할 수도 있다. 예를 들어, Eclipse는 코틀린 플러그인을 설치하면 IntelliJ IDEA와 마찬가지로 코틀린 프로젝트를 지원한다. 프로젝트 시작 시 구성 선택, 디렉토리 구조, 필수 라이브러리 설정 등 세부적인 사항들이 중요하다.

## See Also (참고 자료)
- 코틀린 공식 문서: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- IntelliJ IDEA 다운로드: [https://www.jetbrains.com/idea/download/](https://www.jetbrains.com/idea/download/)
- Eclipse 코틀린 플러그인 정보: [https://marketplace.eclipse.org/content/kotlin-plugin-eclipse](https://marketplace.eclipse.org/content/kotlin-plugin-eclipse)
