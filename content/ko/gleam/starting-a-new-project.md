---
title:                "새 프로젝트 시작하기"
date:                  2024-01-20T18:03:28.920031-07:00
model:                 gpt-4-1106-preview
simple_title:         "새 프로젝트 시작하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
새로운 프로젝트를 시작한다는 건 빈 캔버스에 그림을 그리기 시작하는 것과 같아요. 프로그래머는 새로운 아이디어를 실현하거나, 기술을 연마하기 위해 프로젝트를 시작합니다.

## How to: (어떻게 할까요?)
```gleam
// 프로젝트 시작하기: `gleam new my_project`를 콘솔에 입력
$ gleam new my_project
$ cd my_project
```

샘플 출력:

```shell
Your Gleam project my_project has been successfully created.
The `my_project` directory contains the following files:

* gleam.toml - Project configuration
* src/my_project.gleam - Source file where you can start writing your application
* test/my_project_test.gleam - Test file for your project
```

이제 `src/my_project.gleam` 파일을 편집해서 코드를 작성할 수 있습니다.

## Deep Dive (심층 분석)
Gleam이 등장한 건 타입 안전성과 오류 없는 병렬 처리를 지향하는 언어인 얼랭(Erlang)의 생태계를 간결하고, 빠른 개발이 가능한 현대적 언어로 확장하려는 목표에서 비롯되었습니다. Gleam은 Rust와 Elm을 비롯한 다른 함수형 프로그래밍 언어에서 영감을 받아 타입 안전성과 편리한 개발 경험을 제공해요. 

대안으로는 Elixir나 Erlang 같은 기존 BEAM 언어들을 사용하는 방법이 있지만, Gleam은 더 엄격한 타입 체크와 문법적 간결성 측면에서 차별화를 두고 있습니다.

프로젝트를 실행하려면 `rebar3` 빌드 도구를 사용하고, Gleam은 이를 자동으로 프로젝트에 통합합니다. 단, `rebar3`를 사용하기 전에 Erlang/OTP가 설치되어 있어야 합니다.

## See Also (더 보기)
- Gleam 공식 웹사이트: [https://gleam.run](https://gleam.run)
- Gleam GitHub 저장소: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- Gleam forums for discussions: [https://github.com/gleam-lang/gleam/discussions](https://github.com/gleam-lang/gleam/discussions)