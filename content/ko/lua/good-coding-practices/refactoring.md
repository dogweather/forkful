---
date: 2024-01-26 01:46:57.996669-07:00
description: "\uB9AC\uD329\uD130\uB9C1\uC740 \uAE30\uC874 \uCF54\uB4DC\uB97C \uC218\
  \uC815\uD558\uC5EC \uAD6C\uC870, \uAC00\uB3C5\uC131, \uD6A8\uC728\uC131\uC744 \uAC1C\
  \uC120\uD558\uBA74\uC11C \uC678\uBD80 \uB3D9\uC791\uC744 \uBCC0\uACBD\uD558\uC9C0\
  \ \uC54A\uB294 \uAE30\uC220\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uCF54\uB4DC\uB97C \uB354 \uC720\uC9C0\uBCF4\uC218\uD558\uAE30 \uC27D\uAC8C\
  \ \uB9CC\uB4E4\uACE0, \uBCF5\uC7A1\uC131\uC744 \uC904\uC774\uBA70, \uC0C8\uB85C\uC6B4\
  \ \uAE30\uB2A5\uC744 \uCD94\uAC00\uD558\uAC70\uB098 \uBC84\uADF8\uB97C \uC218\uC815\
  \uD558\uAE30 \uC804 \uC900\uBE44 \uB2E8\uACC4\uB85C \uC790\uC8FC \uC218\uD589\uD569\
  \uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:29.352102-06:00'
model: gpt-4-0125-preview
summary: "\uB9AC\uD329\uD130\uB9C1\uC740 \uAE30\uC874 \uCF54\uB4DC\uB97C \uC218\uC815\
  \uD558\uC5EC \uAD6C\uC870, \uAC00\uB3C5\uC131, \uD6A8\uC728\uC131\uC744 \uAC1C\uC120\
  \uD558\uBA74\uC11C \uC678\uBD80 \uB3D9\uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\
  \uB294 \uAE30\uC220\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uCF54\uB4DC\uB97C \uB354 \uC720\uC9C0\uBCF4\uC218\uD558\uAE30 \uC27D\uAC8C \uB9CC\
  \uB4E4\uACE0, \uBCF5\uC7A1\uC131\uC744 \uC904\uC774\uBA70, \uC0C8\uB85C\uC6B4 \uAE30\
  \uB2A5\uC744 \uCD94\uAC00\uD558\uAC70\uB098 \uBC84\uADF8\uB97C \uC218\uC815\uD558\
  \uAE30 \uC804 \uC900\uBE44 \uB2E8\uACC4\uB85C \uC790\uC8FC \uC218\uD589\uD569\uB2C8\
  \uB2E4."
title: "\uB9AC\uD329\uD130\uB9C1"
---

{{< edit_this_page >}}

## 무엇 & 왜?
리팩터링은 기존 코드를 수정하여 구조, 가독성, 효율성을 개선하면서 외부 동작을 변경하지 않는 기술입니다. 프로그래머들은 코드를 더 유지보수하기 쉽게 만들고, 복잡성을 줄이며, 새로운 기능을 추가하거나 버그를 수정하기 전 준비 단계로 자주 수행합니다.

## 방법:
간단한 Lua 함수를 가져와 리팩터링해보겠습니다. 효율성이나 명확성에 대해 많은 생각 없이 작성된 리스트 내의 숫자들의 합을 계산하는 함수로 시작합니다:

```Lua
function sumList(numbers)
    local result = 0
    for i=1, #numbers do
        for j=1, #numbers do
            if i == j then
                result = result + numbers[i]
            end
        end
    end
    return result
end

print(sumList({1, 2, 3, 4})) -- 출력: 10
```

효율적이고 읽기 쉬운 버전으로 리팩터링:
```Lua
function sumListRefactored(numbers)
    local result = 0
    for _, value in ipairs(numbers) do
        result = result + value
    end
    return result
end

print(sumListRefactored({1, 2, 3, 4})) -- 여전히 출력: 10
```

리팩터링된 버전은 `ipairs`를 사용하여 리스트를 체계적으로 반복함으로써 중복된 내부 루프를 제거합니다.

## 심층 분석
역사적으로, 리팩터링은 80년대 후반 Smalltalk 프로그래밍 커뮤니티에서 비롯되었고, 마틴 파울러의 '리팩터링: 기존 코드의 설계 개선' 책에 의해 대중화되었습니다. Lua에서의 리팩터링은 종종 복잡한 조건 간소화, 대형 함수를 더 작은 함수로 분해, 성능 개선을 위한 테이블 사용 최적화 등을 포함합니다.

Lua에서의 리팩터링은 그만의 주의점이 있습니다; Lua의 동적 특성과 유연한 타이핑은 변수 이름 변경이나 함수 시그니처 변경과 같은 특정 리팩터링 작업을 신중하지 않게 수행할 경우 위험을 초래할 수 있습니다. 정적 코드 분석 도구(`luacheck` 같은)는 이러한 위험을 줄일 수 있습니다. 대안은 테스트 주도 개발(TDD)로, 별도의 리팩터링 단계와 대비되는 개발 과정의 필수적인 부분으로 코드를 지속적으로 리팩터링합니다.

## 참고
- 로베르토 이에루살림스키의 "Programming in Lua"는 최고의 사례와 예시를 위해.
- 마틴 파울러의 "Refactoring: Improving the Design of Existing Code"는 언어를 막론하고 적용 가능한 원칙을 위해.
- Lua 코드의 유지 및 리팩터링을 목적으로 한 도구와 모듈을 위한 LuaRocks 디렉토리 (https://luarocks.org/).
