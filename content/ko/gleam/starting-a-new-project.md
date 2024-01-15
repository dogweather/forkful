---
title:                "새로운 프로젝트 시작하기"
html_title:           "Gleam: 새로운 프로젝트 시작하기"
simple_title:         "새로운 프로젝트 시작하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜?

새로운 프로젝트를 시작하는 이유는 다양합니다. 어떤 사람은 새로운 기술을 배우기 위해, 어떤 사람은 취미 생각으로, 또 어떤 사람은 새로운 제품이나 서비스를 만들기 위해서일 수도 있습니다. Gleam은 안전하고 간결한 함수형 언어로서 새로운 프로젝트를 시작하는 데에 이상적인 선택입니다.

## 만드는 법

Gleam은 간편하고 직관적인 문법을 가지고 있어서 프로그래머들에게 친숙할 것입니다. 다음은 `Hello, World!`를 출력하는 간단한 예제입니다. 

```Gleam
import gleam/io

fn main() {
    io.print("Hello, World!")
}
```

이제 터미널에서 다음 명령어를 입력해보세요.

```
gleam run hello.gleam
```

`Hello, World!`가 성공적으로 출력되었을 것입니다. Gleam은 정적 타입 시스템을 가지고 있어서 에러를 미리 방지할 수 있기 때문에 개발 과정에서도 효율적이고 안전하게 진행할 수 있습니다.

## 딥 다이브

새로운 Gleam 프로젝트를 시작하는 것은 매우 간단합니다. 다음 순서대로 진행하시면 됩니다.

1. Gleam 컴파일러를 설치합니다.
2. 새로운 프로젝트를 위한 디렉토리를 만듭니다.
3. `gleam.toml` 파일을 생성하고 필요한 의존성을 설정합니다.
4. 필요한 파일들을 작성하고 컴파일하고 실행합니다.

더 자세한 정보는 [공식 문서](https://gleam.run/getting-started/)를 참고하세요.

## 더 알아보기

- [Gleam 공식 홈페이지](https://gleam.run/)
- [Gleam GitHub 레포지토리](https://github.com/gleam-lang/gleam)
- [Gleam 커뮤니티 Slack 채널](https://join.slack.com/t/gleam-lang/signup)