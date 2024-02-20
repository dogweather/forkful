---
date: 2024-01-26 00:55:38.490123-07:00
description: "\uCF54\uB529 \uC911 \uC624\uB958 \uCC98\uB9AC\uB294 \uC608\uAE30\uCE58\
  \ \uBABB\uD55C \uC0C1\uD669\uC744 \uC608\uC0C1\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4\
  . \uC774\uB294 \uD504\uB85C\uADF8\uB7A8\uC774 \uC6D0\uD65C\uD558\uAC8C \uC2E4\uD589\
  \uB420 \uC218 \uC788\uB3C4\uB85D \uC77C\uC774 \uC798\uBABB\uB418\uC5C8\uC744 \uB54C\
  \uB97C \uACC4\uD68D\uD558\uB294 \uC608\uC220\uC785\uB2C8\uB2E4."
lastmod: 2024-02-19 22:05:14.340232
model: gpt-4-1106-preview
summary: "\uCF54\uB529 \uC911 \uC624\uB958 \uCC98\uB9AC\uB294 \uC608\uAE30\uCE58 \uBABB\
  \uD55C \uC0C1\uD669\uC744 \uC608\uC0C1\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uC774\
  \uB294 \uD504\uB85C\uADF8\uB7A8\uC774 \uC6D0\uD65C\uD558\uAC8C \uC2E4\uD589\uB420\
  \ \uC218 \uC788\uB3C4\uB85D \uC77C\uC774 \uC798\uBABB\uB418\uC5C8\uC744 \uB54C\uB97C\
  \ \uACC4\uD68D\uD558\uB294 \uC608\uC220\uC785\uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
코딩 중 오류 처리는 예기치 못한 상황을 예상하는 것입니다. 이는 프로그램이 원활하게 실행될 수 있도록 일이 잘못되었을 때를 계획하는 예술입니다.

## 사용 방법:
Lua는 오류 처리를 위해 `pcall`과 `xpcall` 두 가지 주요 함수를 사용합니다. 다음은 그 사용 방법입니다:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("오류! 뭔가 잘못되었습니다.")
    else
        print("모든 것이 잘 되었습니다!")
    end
end

-- pcall 사용하기
local success, errorMessage = pcall(might_fail)

if success then
    print("성공!")
else
    print("오류 발생:", errorMessage)
end

-- 오류 처리기와 함께 xpcall 사용하기
function myErrorHandler(err)
    print("오류 처리기가 말하길:", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("호출이 성공적이었나요?", status)
```

출력 예시:

```
오류 발생: 오류! 뭔가 잘못되었습니다.
오류 처리기가 말하길: 오류! 뭔가 잘못되었습니다.
호출이 성공적이었나요? false
```
또는 오류가 발생하지 않은 경우:
```
모든 것이 잘 되었습니다!
성공!
모든 것이 잘 되었습니다!
호출이 성공적이었나요? true
```

## 심층 탐구
오류 처리 또는 "예외 처리"는 항상 있는 것은 아니었습니다. 초기 프로그램들은 자주 충돌했습니다. 코딩이 발전함에 따라 안정성에 대한 필요성도 발전했습니다. Lua의 접근법은 일부 언어에 비해 단순합니다. `try/catch` 블록은 없고, `pcall`과 `xpcall`만 있습니다. 전자는 함수 호출을 보호하여 상태와 오류를 반환합니다. 후자는 오류 처리 기능을 추가하여, 사용자 정의 정리 또는 로깅에 유용합니다.

Lua에서의 한 대안은 `assert`를 사용하는 것인데, 이는 그 조건이 거짓인 경우 오류를 발생시켜 유사한 목적을 제공할 수 있습니다. 그러나 복잡한 오류 처리 시나리오에는 `pcall`만큼 유연하지 않습니다.

내부적으로 `pcall`과 `xpcall`은 함수가 실행될 수 있는 "보호된 환경"을 설정하는 방식으로 작동합니다. 오류가 발생하면 환경이 그것을 잡아 현장에서 처리하거나 프로그램이 처리할 수 있도록 돌려보낼 수 있습니다.

## 참조
- Lua 코딩에 관한 책 (세 번째 에디션), 오류 처리에 대한 철저한 내용을 읽으려면 https://www.lua.org/pil/ (8.4 절)을 참조하세요.
- 공식 Lua 5.4 참조 매뉴얼: https://www.lua.org/manual/5.4/ - Lua의 오류 처리 함수에 관한 가장 최신 정보를 위해.
- Lua 사용자 위키의 오류 처리: http://lua-users.org/wiki/ErrorHandling - 커뮤니티 통찰과 패턴을 위해.
