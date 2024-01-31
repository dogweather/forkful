---
title:                "새 프로젝트 시작하기"
date:                  2024-01-20T18:03:48.988858-07:00
model:                 gpt-4-1106-preview
simple_title:         "새 프로젝트 시작하기"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
새 프로젝트를 시작한다는 것은 공백 상태에서 아이디어를 실현하려는 과정입니다. 프로그래머들은 새로운 문제를 해결하거나, 기능을 개선하기 위하여 새 프로젝트를 만듭니다.

## How to: (방법)
Let's start a new Haskell project with Stack. First, install Stack if you haven't yet:

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Create a new project:

```bash
stack new myproject
cd myproject
```

Build and run your project:

```bash
stack build
stack exec myproject-exe
```

Sample output might look like:

```
Hello, Haskell!
```

## Deep Dive (심화 탐구)
Haskell은 1990년대 초에 개발된 고차원 함수 언어입니다. '스택(Stack)'을 사용해 새 프로젝트를 시작하는 것은 의존성 관리를 자동화하고 프로젝트 설정을 표준화하는 데 도움을 줍니다. 대안으로는 'Cabal'이 있지만, Stack이 더 널리 사용됩니다. Stack은 프로젝트가 해당 환경에 맞는 GHC(Glasgow Haskell Compiler) 버전을 사용하도록 보장하며, 프로젝트 내 모든 패키지 버전이 맞도록 관리해줍니다.

## See Also (관련 링크)
- Stack's official website: [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)
- Learn more about Haskell: [http://learnyouahaskell.com/](http://learnyouahaskell.com/)
- Haskell Programming Language official website: [https://www.haskell.org/](https://www.haskell.org/)

Remember, practice is key in Haskell, like in any other language. 즐겁게 코드를 작성하세요!
