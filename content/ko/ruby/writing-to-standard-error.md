---
title:    "Ruby: 표준 에러에 쓰는 방법"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜

표준 오류에 쓰기 작성하는 것의 중요성은 디버깅 및 코드 오류 해결에 있습니다. 이를 통해 러비 프로그래밍에서 발생하는 문제를 더 쉽게 식별하고 해결할 수 있습니다.

## 방법

표준 오류에 쓰기 작성하는 방법은 매우 간단합니다. 우선, `STDERR` 객체를 사용하고 `write` 메소드를 호출하여 오류 메시지를 작성합니다. 아래는 간단한 예제 코드와 그에 해당하는 출력입니다.

```Ruby
STDERR.write "이번 코드에서 오류가 발생했습니다."
```

```
이번 코드에서 오류가 발생했습니다.
```

## 깊이 파고들기

표준 오류에 쓰기 작성하는 것은 간단해 보일 수 있지만, 실제로는 매우 유용한 기능입니다. `STDERR` 객체는 오류 메시지만 아니라 디버그 정보나 경고 메시지를 작성하는데에도 사용할 수 있습니다. 또한 `puts` 메소드로 작성한 내용은 `STDOUT`에 출력되지만, `STDERR`에 작성한 내용은 터미널에서 더 쉽게 식별할 수 있기 때문에 디버깅에 유용합니다.

# 또 보기

[Writing to Standard Error in Ruby](https://www.rubyguides.com/2019/10/stderr-stdout-ruby/)  
[Ruby STDERR class](https://www.tutorialspoint.com/ruby/ruby_stderr.htm)