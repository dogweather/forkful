---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:53.973779-07:00
description: "\uD45C\uC900 \uC624\uB958(stderr)\uC5D0 \uC4F0\uAE30\uB294 \uC624\uB958\
  \ \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8 \uCD9C\uB825\uC744 \uD45C\uC900 \uCD9C\uB825\
  (stdout)\uACFC \uB2E4\uB978 \uBCC4\uB3C4\uC758 \uCC44\uB110\uB85C \uC804\uC1A1\uD558\
  \uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC77C\uBC18 \uD504\uB85C\uADF8\uB7A8 \uACB0\uACFC\uC640 \uC624\
  \uB958 \uC815\uBCF4\uB97C \uAD6C\uBCC4\uD558\uACE0, \uB514\uBC84\uAE45 \uBC0F \uB85C\
  \uAE45 \uD504\uB85C\uC138\uC2A4\uB97C \uAC04\uC18C\uD654\uD558\uAE30 \uC704\uD574\
  \ \uC774\uB807\uAC8C \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.441280-06:00'
model: gpt-4-0125-preview
summary: "\uD45C\uC900 \uC624\uB958(stderr)\uC5D0 \uC4F0\uAE30\uB294 \uC624\uB958\
  \ \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8 \uCD9C\uB825\uC744 \uD45C\uC900 \uCD9C\uB825\
  (stdout)\uACFC \uB2E4\uB978 \uBCC4\uB3C4\uC758 \uCC44\uB110\uB85C \uC804\uC1A1\uD558\
  \uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4."
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
weight: 25
---

## 방법:
Lua에서는 `io.stderr:write()` 함수를 사용하여 stderr에 쓸 수 있습니다. 다음은 간단한 오류 메시지를 표준 오류에 쓰는 방법입니다:

```lua
io.stderr:write("Error: Invalid input.\n")
```

변수를 출력하거나 여러 데이터 조각을 결합해야 하는 경우, write 함수 내에서 이들을 연결하세요:

```lua
local errorMessage = "Invalid input."
io.stderr:write("Error: " .. errorMessage .. "\n")
```

**stderr에서의 샘플 출력:**
```
Error: Invalid input.
```

더 복잡한 시나리오나 더 큰 애플리케이션을 다룰 때는 LuaLogging과 같은 타사 로깅 라이브러리를 고려할 수 있습니다. LuaLogging을 사용하면 로그를 stderr를 포함한 다양한 목적지로 지정할 수 있습니다. 다음은 간략한 예시입니다:

먼저, LuaRocks를 사용하여 LuaLogging이 설치되어 있는지 확인하세요:

```
luarocks install lualogging
```

그런 다음, LuaLogging을 사용하여 stderr에 오류 메시지를 작성하려면:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Error: Invalid input.")
```

이 접근 방식은 간단한 API를 통해 로그 레벨(예: ERROR, WARN, INFO)을 설정하는 추가적인 유연성을 제공하면서, 애플리케이션 전반에 걸쳐 표준화된 로깅의 이점을 제공합니다.
