---
date: 2024-01-20 18:03:36.548197-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C?) \uC0C8 \uD504\uB85C\uC81D\uD2B8\uB97C\
  \ \uB9CC\uB4E4 \uB54C \uB098\uD0C0\uB098\uB294 \uD3F4\uB354 \uAD6C\uC870\uC640 \uAE30\
  \uBCF8 \uD30C\uC77C\uB4E4\uC740 \uC774\uB807\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.557653-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C?) \uC0C8 \uD504\uB85C\uC81D\uD2B8\uB97C \uB9CC\uB4E4\
  \ \uB54C \uB098\uD0C0\uB098\uB294 \uD3F4\uB354 \uAD6C\uC870\uC640 \uAE30\uBCF8 \uD30C\
  \uC77C\uB4E4\uC740 \uC774\uB807\uC2B5\uB2C8\uB2E4."
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
weight: 1
---

## How to: (어떻게?)
```elixir
# Elixir 설치 후, Mix에 포함된 명령어로 새 프로젝트를 만드는 방법

# 새 프로젝트 생성
mix new my_project

# 생성된 프로젝트 디렉토리로 이동
cd my_project

# 의존성을 가져오고 컴파일하기
mix deps.get
mix compile

# 애플리케이션 실행
iex -S mix
```

새 프로젝트를 만들 때 나타나는 폴더 구조와 기본 파일들은 이렇습니다:
```
my_project/
├── _build/
├── config/
├── lib/
│   └── my_project.ex
├── test/
│   └── test_helper.exs
│   └── my_project_test.exs
├── mix.exs
└── README.md
```

## Deep Dive (깊이 알아보기)
Elixir는 Erlang VM (BEAM) 위에서 돌아가며, 높은 동시성과 장애 내성을 가진 애플리케이션 개발에 강점이 있습니다. Mix는 Elixir의 빌드 도구로, 프로젝트 시작부터 테스팅, 배포까지 지원해요. 다른 언어에서 `npm`이나 `bundle` 같은 역할이죠.

Elixir가 등장하기 전, Erlang은 같은 목적을 가진 프로젝트들 사이에서 코드를 공유하는 데 한계가 있었어요. Elixir와 Mix가 등장하면서, 프로젝트 생성이 더 간편해지고, 라이브러리 관리가 더 수월해졌죠.

Elixir 프로젝트를 시작할 때 대안으로는 Phoenix 프레임워크를 사용하는 방법도 있어요. Phoenix는 웹 개발에 특화된 프레임워크입니다. Elixir 프로젝트의 시작점으로 좋은 선택이 될 수 있어요.

## See Also (더 보기)
- [Elixir 공식 문서](https://elixir-lang.org/docs.html)
- [Mix & OTP 안내서](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
- [Phoenix 프로젝트 시작하기](https://www.phoenixframework.org/)
- [Elixir 포럼](https://elixirforum.com/)

이 글을 통해 Elixir와 그 시작 과정에 대한 이해가 조금 더 쉬워졌길 바라며, 즐거운 코딩 되세요!
