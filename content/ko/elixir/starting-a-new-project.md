---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
새 프로젝트를 시작하는 것은 새로운 아이디어를 실현하거나 문제를 해결하기 위해 프로그램을 제작하는 과정을 말합니다. 프로그래머는 창조적인 생각을 현실로 만들거나, 기존 문제를 해결하기 위해 새 프로젝트를 시작합니다.

## 어떻게: 
새로운 Elixir 프로젝트를 시작하기 위해, `mix new` 명령어를 사용합니다. 프로젝트 이름을 지정하고, 필요한 경우 `--sup` 플래그를 추가하여 Supervision tree를 생성할 수 있습니다.

```Elixir
$ mix new my_project
$ cd my_project
```

Elixir 셸에서 응용 프로그램을 시작하는 방법은 다음과 같습니다.

```Elixir
$ iex -S mix
```

이렇게 하면 `my_project` 폴더에 프로젝트의 기본 구조가 생성됩니다.

## 깊이 들여다 보기:
Elixir에서 프로젝트를 시작하는 기능은 Elixir의 빌드 도구인 Mix를 사용합니다. Mix는 프로젝트를 구성하고 테스트하며, 배포하는 데 필요한 모든 도구를 제공합니다. 

대안으로, Elixir와 함께 사용할 수 있는 다른 빌드 도구들도 있습니다. 예를 들어, Rebar3라는 Erlang 빌드 도구는 Elixir 프로젝트에도 사용할 수 있습니다.

Elixir 프로젝트를 시작할 때, Mix는 프로젝트의 디펜던시, 구성, 애플리케이션 파일을 정의하는 mix.exs라는 특별한 파일을 생성합니다. 이 파일은 프로젝트에 포함된 모듈과 함수에 대한 규정 등 프로젝트의 핵심 세부사항을 담고 있습니다.

## 추가 정보: 
- [Elixir 공식 문서](https://elixir-lang.org/docs.html)
- [Mix를 사용한 프로젝트 관리](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
- [Erlang 빌드 도구 Rebar3](https://rebar3.org/docs/)
- [Elixir School: Project 설정](https://elixirschool.com/ko/lessons/basics/mix/)