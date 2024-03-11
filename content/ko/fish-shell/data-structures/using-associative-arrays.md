---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:02.727424-07:00
description: "\uC5F0\uAD00 \uBC30\uC5F4 \uB610\uB294 \uD574\uC2DC \uB9F5\uC744 \uC0AC\
  \uC6A9\uD558\uBA74 \uB370\uC774\uD130\uB97C \uD0A4-\uAC12 \uC30D\uC73C\uB85C \uC800\
  \uC7A5\uD558\uC5EC \uD0A4\uB85C \uC815\uBCF4\uB97C \uB354 \uC27D\uAC8C \uAD6C\uC131\
  \uD558\uACE0 \uAC80\uC0C9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBAA9\uB85D\uB9CC\
  \ \uC0AC\uC6A9\uD558\uB294 \uAC83\uBCF4\uB2E4 \uB370\uC774\uD130\uB97C \uCC98\uB9AC\
  \uD560 \uB54C \uB354 \uAD6C\uC870\uD654\uB41C \uBC29\uBC95\uC774 \uD544\uC694\uD560\
  \ \uB54C, \uD2B9\uD788 \uC124\uC815\uACFC \uB2E4\uC591\uD55C \uC18D\uC131\uC744\
  \ \uB2E4\uB8F0 \uB54C \uC720\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-11T00:14:29.767599-06:00'
model: gpt-4-0125-preview
summary: "\uC5F0\uAD00 \uBC30\uC5F4 \uB610\uB294 \uD574\uC2DC \uB9F5\uC744 \uC0AC\uC6A9\
  \uD558\uBA74 \uB370\uC774\uD130\uB97C \uD0A4-\uAC12 \uC30D\uC73C\uB85C \uC800\uC7A5\
  \uD558\uC5EC \uD0A4\uB85C \uC815\uBCF4\uB97C \uB354 \uC27D\uAC8C \uAD6C\uC131\uD558\
  \uACE0 \uAC80\uC0C9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBAA9\uB85D\uB9CC \uC0AC\
  \uC6A9\uD558\uB294 \uAC83\uBCF4\uB2E4 \uB370\uC774\uD130\uB97C \uCC98\uB9AC\uD560\
  \ \uB54C \uB354 \uAD6C\uC870\uD654\uB41C \uBC29\uBC95\uC774 \uD544\uC694\uD560 \uB54C\
  , \uD2B9\uD788 \uC124\uC815\uACFC \uB2E4\uC591\uD55C \uC18D\uC131\uC744 \uB2E4\uB8F0\
  \ \uB54C \uC720\uC6A9\uD569\uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

연관 배열 또는 해시 맵을 사용하면 데이터를 키-값 쌍으로 저장하여 키로 정보를 더 쉽게 구성하고 검색할 수 있습니다. 목록만 사용하는 것보다 데이터를 처리할 때 더 구조화된 방법이 필요할 때, 특히 설정과 다양한 속성을 다룰 때 유용합니다.

## 방법:

Fish는 Bash 4+처럼 기본적으로 연관 배열을 지원하지 않지만, 리스트와 문자열 조작의 조합을 사용하여 비슷한 기능을 달성할 수 있습니다. 연관 배열을 모방하는 방법은 다음과 같습니다:

먼저 "연관 배열" 요소를 각각 설정합니다:

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

요소에 접근하려면 직접 참조하기만 하면 됩니다:

```Fish Shell
echo $food_color_apple
# 출력: red
```

반복해야 하는 경우, 이름 규칙을 고려하여 for-loop를 사용합니다:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# 출력:
# red
# yellow
```

Bash의 `${!array[@]}`로 모든 키를 가져오는 기능이 없는 경우, 별도의 리스트에 키를 저장할 수 있습니다:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'is' $food_color_$key
end
# 출력:
# apple is red
# banana is yellow
```

## 심층 분석

다른 스크립팅 언어에서처럼 진정한 연관 배열은 아직 Fish의 접근 방식의 일부가 아닙니다. 보여진 해결책은 Fish의 문자열 조작 및 리스트 기능을 활용하여 가상의 연관 배열 구조를 생성합니다. 이 방법은 작동하지만, 내장 연관 배열 지원이 있을 때보다 깔끔하거나 오류에 강하지 않습니다. Bash와 Zsh와 같은 다른 쉘들은 내장 연관 배열 기능을 제공하여 보다 직관적이고 읽기 쉬운 코드를 가능하게 합니다. 그러나, Fish의 설계 철학은 단순성과 사용자 친화성을 추구하는 것으로, 이러한 기능을 포기하는 대가일 수 있습니다. 이 해결책은 대부분의 요구를 충족시키지만, Fish Shell의 발전에 주목하세요—개발자들은 커뮤니티의 피드백을 바탕으로 기능을 개선하고 추가하는 데 적극적입니다.
