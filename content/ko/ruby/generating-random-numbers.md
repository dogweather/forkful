---
title:                "난수 생성하기"
date:                  2024-01-20T17:50:16.022513-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

랜덤 숫자를 생성하는 것은 예측할 수 없는 번호를 만드는 과정입니다. 이는 게임, 보안, 데이터 분석 등 다양한 프로그래밍 상황에서 중요합니다.

## How to: (어떻게:)

```Ruby
# Ruby에서의 기본적인 랜덤 숫자 생성

# 0 이상 10 미만의 랜덤 숫자
puts rand(10)

# 1 이상 10 이하의 랜덤 숫자
puts rand(1..10)

# 랜덤한 실수 생성
puts rand

# 시드 설정으로 재현 가능한 랜덤 숫자 시퀀스
srand 1234
puts rand
srand 1234
puts rand
```
샘플 출력:
```
5
3
0.4738372337607345
0.1915194503788923
0.1915194503788923
```

## Deep Dive (심층 탐구)

랜덤 숫자 생성은 1940년대부터 시작되었습니다. 당시 연구자들이 몬테카를로 방법을 사용할 때 중요했습니다. Ruby에서 `Random` 클래스는다양한 방식으로 랜덤 넘버를 생성할 수 있습니다. 옵션으로, `SecureRandom` 모듈은 보안이 중요한 애플리케이션에 권장됩니다.

Ruby의 표준 랜덤 넘버 생성기는 메르센 트위스터(Mersenne Twister) 알고리즘을 사용합니다. 이 알고리즘은 빠르면서도 매우 긴 주기를 가지며 고품질의 난수를 생성하는 것으로 유명합니다. 하지만, 암호화 목적에는 부적합하다는 점을 기억하세요.

```Ruby
require 'securerandom'

# SecureRandom을 사용하는 예시
puts SecureRandom.hex(10)
```

위 `SecureRandom` 예시는 암호화에 적합한 보안 랜덤 번호를 생성합니다. 이는 웹 토큰이나 세션 ID를 생성할 때 유용합니다.

## See Also (더 보기)

- Ruby 공식 문서: [Random class](https://ruby-doc.org/core-3.1.2/Random.html)
- Ruby 공식 문서: [SecureRandom module](https://ruby-doc.org/stdlib-3.1.2/libdoc/securerandom/rdoc/SecureRandom.html)
- Wikipedia: [Monte Carlo method](https://en.wikipedia.org/wiki/Monte_Carlo_method)
- Wikipedia: [Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)
