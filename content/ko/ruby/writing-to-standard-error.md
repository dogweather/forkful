---
title:    "Ruby: 표준 오류에 쓰는 방법"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## 왜

Ruby 프로그래밍을 하는 이유는 다양합니다. 하지만 그 중에서도 가장 중요한 이유 중 하나는 바로 표준 오류 출력을 작성하는 것입니다. 표준 오류 출력은 디버깅과 에러 처리를 위해 매우 유용합니다. 이 블로그 포스트에서는 Ruby에서 표준 오류 출력을 작성하는 방법에 대해 알아보도록 하겠습니다.

## 방법

표준 오류 출력을 작성하는 방법은 매우 간단합니다. 먼저 `STDERR.puts` 메소드를 사용하여 문자열을 출력할 수 있습니다.

```Ruby
STDERR.puts "이것은 표준 오류 출력입니다."
```

위의 코드를 실행하면 이 문자열이 터미널에 출력됩니다.

```
이것은 표준 오류 출력입니다.
```

또한, `STDERR` 객체를 직접 사용하여 오류 메시지를 출력할 수도 있습니다.

```Ruby
STDERR.write "에러가 발생했습니다."
```

위의 코드는 줄 바꿈 없이 오류 메시지를 출력합니다.

```
에러가 발생했습니다.
```

## 깊이 들어가보기

Ruby에서는 표준 오류 출력을 다양한 방법으로 사용할 수 있습니다. 예를 들어, `raise` 메소드를 사용하여 에러를 발생시켜 표준 오류 출력에 에러 메시지를 출력하는 것도 가능합니다.

```Ruby
raise StandardError, "에러가 발생했습니다."
```

이렇게 하면 프로그램이 에러를 발생시키고 오류 메시지를 터미널에서 확인할 수 있습니다.

표준 오류 출력은 또한 디버깅을 위해 매우 중요한 역할을 합니다. 표준 출력을 사용하는 것보다 표준 오류 출력을 사용하면 버그를 찾고 수정하는 과정이 훨씬 더 쉬워집니다.

## 더 알아보기

표준 오류 출력에 대해 더 자세한 내용은 다음 링크를 확인해보세요.

- [Ruby 표준 라이브러리 문서](https://ruby-doc.org/core-2.7.3/IO.html#method-i-puts)
- [컴퓨터 프로그래밍에서의 표준 출력과 표준 오류 출력](https://ko.wikipedia.org/wiki/%ED%91%9C%EC%A4%80_%EC%B6%9C%EB%A0%A5%EA%B3%BC_%ED%91%9C%EC%A4%80_%EC%98%A4%EB%A5%98_%EC%B6%9C%EB%A0%A5)
- [표준 입출력에 대해 자세히 알아보기](https://blog.eunsatio.io/tech/2019/06/11/std-pipe.html)

## 또 다른 링크

- [Ruby로 표준 출력하기](https://github.com/joinc-rb/rails/blob/master/guides/source/debugging_rails_applications.md#%EC%84%9C%EB%B9%84%EC%8A%A4-%EC%A0%80%EC%9E%A5-%EC%B6%9C%EB%A0%A5%ED%95%98%EA%B8%B0)
- [Ruby Debugging Techniques](https://www.toptal.com/ruby/ruby-debugging-tips-and-tricks)