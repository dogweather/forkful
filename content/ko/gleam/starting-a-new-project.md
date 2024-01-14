---
title:    "Gleam: 새 프로젝트 시작하기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

Gleam 프로그래밍 블로그 포스트 - 새 프로젝트 시작하기

## 왜?

새로운 프로젝트를 시작할 때 왜 누군가는 이에 참여하는지 궁금하지 않으신가요? Gleam은 에러가 더 적은 코드를 작성하고 디버깅하기 쉬우며, 함수형 프로그래밍과 모듈 시스템을 지원하기 때문에 더 나은 개발 경험을 제공합니다. 또한 Erlang 가상 머신 위에서 돌아가기 때문에 견고하고 확장 가능한 어플리케이션을 만들 수 있습니다.

## 시작하는 방법

Gleam은 Erlang와 유사한 문법을 사용하기 때문에 Erlang 배경지식이 있는 분이라면 금방 익힐 수 있습니다. 그렇지 않더라도 Gleam 문서를 참고하면서 코드를 작성하면 됩니다.

먼저 새로운 프로젝트 디렉토리를 만든 다음, 그 안에 `src` 폴더를 만듭니다. 그리고 `src` 폴더 안에 `main.gleam` 파일을 만듭니다. 이 파일에 다음과 같이 작성해보세요.

```Gleam
pub fn main() {
  let message = "New project, who dis?"
  IO.println(message)
}
```

이제 터미널에서 `gleam build src/main.gleam`을 실행해보면 컴파일이 성공하고 `new_project` 바이너리 파일이 생성됩니다. 이제 `./new_project`를 실행하면 메시지가 출력됩니다.

## 깊게 파보기

새로운 Gleam 프로젝트를 시작할 때 몇 가지 중요한 점을 주의해야 합니다. 첫째, `pub fn main` 함수는 모든 프로젝트에서 유일해야 합니다. 둘째, 모든 코드는 `src` 폴더에 있어야 합니다. 셋째, `pub fn main` 함수는 어떤 값을 반환하지 않고, 모든 코드는 어떤 값이든 반환해야 합니다.

## 관련 자료

Gleam 문서: https://gleam.run/getting-started.html

Erlang 문서: http://erlang.org/doc/index.html

IO 모듈 문서: https://gleam.run/modules/io.html

파일 I/O 예제: https://github.com/gleam-lang/gleam/blob/main/examples/file_io.gleam

홈브루를 통한 Gleam 설치: https://gleam.run/getting-started.html#installing-gleam-with-homebrew

오마츠 캠페인 - 함수형 언어 블로거: https://gleam.run/posts/erlang-and-everything-austin-zhu-has-learned.html

Youtobe - Gleam 패턴 매칭 예제: https://www.youtube.com/watch?v=fFcIvt1vfwY