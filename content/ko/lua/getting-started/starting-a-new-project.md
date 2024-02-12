---
title:                "새 프로젝트 시작하기"
aliases:
- /ko/lua/starting-a-new-project.md
date:                  2024-01-20T18:04:01.822564-07:00
model:                 gpt-4-1106-preview
simple_title:         "새 프로젝트 시작하기"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)
프로젝트를 시작한다는 건, 빈 문서에서부터 코드를 차곡차곡 쌓아감을 의미합니다. 프로그래머들이 새로운 프로젝트를 시작하는 이유는 문제를 해결하고, 아이디어를 실현하며, 능력을 키우기 위해서죠.

## How to: (어떻게:)
Lua는 간단하고 유연한 언어입니다. 새 프로젝트를 시작하려면, 기본적인 Lua 파일을 만들어 봅시다.

```Lua
-- main.lua 파일
print("안녕, 새 프로젝트!")

function greet(name)
    print("안녕, " .. name .. "!")
end

greet("Lua")
```

샘플 출력:
```
안녕, 새 프로젝트!
안녕, Lua!
```

## Deep Dive (심층 탐구)
Lua는 1993년에 브라질에서 탄생했습니다. 게임 개발, 웹 서버 스크립팅, 데스크톱 애플리케이션에서 널리 쓰여요. Lua는 C 언어로 쓰였기 때문에 여러 플랫폼에서 쉽게 실행됩니다. 다른 언어로 프로젝트를 시작하는 것도 가능하지만, Lua는 학습하기 쉽고, 강력한 API 연동 능력을 가진 언어입니다.

## See Also (더 보기)
- Lua 공식 홈페이지: [https://www.lua.org](https://www.lua.org)
- Lua 프로그래밍에 대한 튜토리얼: [http://www.lua.org/pil/contents.html](http://www.lua.org/pil/contents.html)
