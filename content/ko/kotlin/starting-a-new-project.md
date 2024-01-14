---
title:                "Kotlin: 새로운 프로젝트 시작하기"
programming_language: "Kotlin"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜: 새로운 프로젝트를 시작하는데 동기를 부여하는 이유

새로운 프로젝트를 시작해보는 것은 스스로를 도전하고 발전시키기 위한 좋은 방법입니다. 새로운 언어나 기술을 배우며 새로운 경험을 쌓을 수 있으며, 자신의 창의성을 발휘할 수 있는 기회가 될 것입니다.

## 이렇게 해봐요: 새로운 프로젝트를 시작하는 방법

우선, 코틀린 언어의 기본 문법을 익힌 후 새로운 프로젝트를 만들기 위해 일반적으로 사용하는 빌드 도구를 사용해야합니다. 여기서는 Gradle을 예시로 들겠습니다.

```Kotlin
// build.gradle 파일

plugins {
  id 'org.jetbrains.kotlin.jvm' version '1.5.21'
}

repositories {
  mavenCentral()
}

dependencies {
  implementation 'org.jetbrains.kotlin:kotlin-stdlib-jdk8'
}
```

여기서는 코틀린 표준 라이브러리를 사용하고 있으며, 다른 의존성을 추가해야할 경우 위의 `dependencies` 섹션에 추가하면 됩니다. 또한, 새로운 프로젝트를 시작할 때는 IntelliJ IDEA와 같은 통합 개발 환경(IDE)를 사용하는 것이 좋습니다. 이를 통해 코틀린 언어와 관련된 많은 도구와 기능을 사용할 수 있습니다.

## 깊이 들어가보기: 새로운 프로젝트를 시작함에 있어 더 알아봐야 할 것들

새로운 프로젝트를 시작하기 전에 목표를 설정하고, 그에 맞는 디자인 패턴과 아키텍처를 고민하는 것이 중요합니다. 또한, 적절한 버전 관리 시스템을 사용하여 프로젝트의 변경사항을 관리하고, 팀원과 함께 협업할 수 있도록 해야합니다. 프로젝트를 시작하기 전에 이러한 기본적인 사항들을 고려하는 것이 좋습니다.

## 더 알아보기: 관련된 링크들

- [Kotlin 공식 문서](https://kotlinlang.org/docs/home.html)
- [코틀린 클라우드 개발 오픈 소스 프로젝트](https://cloudnative.to/)
- [Kotlin 개발자 커뮤니티 포럼](https://discuss.kotlinlang.org/)
- [코틀린을 활용한 안드로이드 앱 개발 가이드](https://developer.android.com/kotlin)
- [코틀린으로 시작하는 함수형 프로그래밍](https://docongminh.com/blog/kotlin-fp-introduction/)