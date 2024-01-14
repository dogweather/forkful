---
title:    "Ruby: 랜덤 숫자 생성하기"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜
랜덤한 숫자를 생성하는 것에 관심을 가지게 되는 이유는 데이터 분석, 게임 프로그래밍, 보안 등 다양한 분야에서 중요한 역할을 할 수 있기 때문입니다.

## 생성하는 방법
```Ruby
# 1에서 10 사이의 랜덤한 정수 생성하기
rand(1..10)

# 배열에서 랜덤한 요소 선택하기
[1, 2, 3, 4, 5].sample

# 주어진 범위에서 랜덤한 실수 생성하기
rand(10.0..20.0)

# 시드 값 설정하기
srand(1234)
rand(1..10) # 항상 같은 숫자가 나오게 됨
```

샘플 코드를 실행하면 다양한 조건에서 랜덤한 숫자를 생성할 수 있음을 확인할 수 있습니다. 이런 기술을 사용하면 게임에서 무작위로 이벤트를 발생시키거나, 데이터 분석에서 샘플링을 할 때 유용하게 사용할 수 있습니다.

## 더 깊이 알아보기
랜덤한 숫자를 생성하기 위해서는 컴퓨터 내부에서 난수를 생성하는 알고리즘이 중요합니다. 일반적으로 사용되는 알고리즘은 선형 합동방법 (Linear Congruential Method) 이며, 시드 값과 고정된 상수 값을 이용하여 난수를 생성합니다. 

랜덤한 숫자를 생성할 때 우리가 직접 시드 값을 설정하는 경우가 많습니다. 이는 개발 및 디버깅 과정에서 동일한 값이 나오도록 하기 위함입니다. 하지만 실제 서비스에서는 우리가 직접 시드 값을 설정할 수 없기 때문에 컴퓨터 시간이나 사용자의 주로 중복되지 않는 값을 시드 값으로 사용합니다.

# 참고 자료
[랜덤한 숫자 생성하기 - Ruby 공식 문서](https://ruby-doc.org/core-2.7.1/Random.html)

[Creating Random Numbers in Ruby - The Pragmatic Studio](https://online.pragmaticstudio.com/tour/courses/ruby/random-numbers-in-ruby)

기타 다양한 Ruby 관련 자료는 [Ruby 한국 유저 그룹](https://ruby-korea.github.io)에서 찾아볼 수 있습니다.