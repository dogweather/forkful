---
title:                "임의 숫자 생성"
html_title:           "Ruby: 임의 숫자 생성"
simple_title:         "임의 숫자 생성"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

```
## What & Why?
난수 생성이란 무엇인가요? 그리고 왜 프로그래머들이 이 작업을 할까요? 
난수 생성은 프로그램이나 알고리즘에서 사용될 목적으로 무작위로 생성되는 숫자를 말합니다. 이는 데이터 분석, 암호학 등 다양한 분야에서 사용될 수 있습니다. 난수 생성은 소프트웨어 디자인에서 중요한 요소이며 우리가 만나게 되는 암호나 보안 관련 기능들은 대부분 난수 생성을 기반으로 합니다. 

## How to:
먼저 ```Kernel#rand``` 메소드를 이용하여 Ruby에서 난수를 생성할 수 있습니다. 아래의 코드는 1에서 10 사이의 무작위 숫자를 생성합니다.
```
rand(1..10)
 => 1  # 예시 출력값
```

해당 메소드는 인자로 Range 객체나 최대값을 입력받을 수 있습니다. 아래의 예시는 20 이하의 무작위 수를 생성하는 방법입니다.
```
rand(0..20)
 => 8 # 예시 출력값
```

```Random``` 클래스를 이용하여도 난수를 생성할 수 있습니다. 이 클래스는 별도의 인스턴스를 생성하여 난수를 생성하거나, 클래스 메소드로 바로 생성할 수 있습니다. 아래의 코드는 ```Random```을 사용해서 1에서 100 사이의 난수를 생성하는 예시입니다.
```
Random.new.rand(1..100)
 => 35 # 예시 출력값
```

## Deep Dive:
난수 생성은 컴퓨터 과학에서 오래전부터 사용되고 있었습니다. 그 중에서도 가장 유명한 알고리즘은 선형 합동법(Linear Congruential Method)입니다. 그러나 이 알고리즘은 통계적으로 난수를 생성하지 못해 사용이 권장되지 않습니다. 다양한 알고리즘들이 존재하지만 최적의 난수 생성 방법은 아직도 연구 중에 있습니다. 

일반적으로 난수 생성은 온라인에서 접근할 수 있는 난수 생성기를 사용하거나 운영체제나 하드웨어에서 제공하는 난수 생성 모듈을 사용합니다. 이러한 모듈들은 보안이 중요한 시스템에서 사용될 때에도 높은 수준의 난수를 생성할 수 있도록 설계되어 있습니다.

## See Also:
- [Kernel Ruby කිරීමට භාවිතා කිරීම](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-rand): Ruby에서 난수 생성을 위해 제공하는 빌트인 메소드에 대한 문서입니다.
- [Random Ruby කිරීමට භාවිතා කිරීම](https://ruby-doc.org/stdlib-2.7.1/libdoc/securerandom/rdoc/Random.html): ```Random``` 클래스에 대한 문서입니다.
- [난수 생성기 순위](https://btoonstudio.tistory.com/57): 유명한 난수 생성기들의 순위를 볼 수 있는 블로그 글입니다.