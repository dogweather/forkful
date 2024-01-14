---
title:    "PHP: 텍스트를 검색하고 교체하기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜
누군가가 텍스트를 검색하고 대체하는 것에 참여하는 이유는 코드를 유지하고 편집할 때 편리하고 빠르게 작업할 수 있기 때문입니다.

## 방법
PHP에서 텍스트를 검색하고 대체하는 방법은 다음과 같습니다. 먼저, `str_replace()` 함수를 사용하여 원하는 텍스트를 찾아 다른 텍스트로 대체합니다. 예를 들어, 다음과 같은 변수가 있다고 가정해봅시다.
```PHP
  $text = "안녕하세요, PHP를 배우는 건 어떠신가요?";
  echo str_replace("배우는", "공부하는", $text);
```

위의 코드는 `$text` 변수에서 "배우는"을 찾아 "공부하는"으로 대체하는 것을 보여줍니다. 아래의 출력을 볼 수 있습니다.
```
안녕하세요, PHP를 공부하는 건 어떠신가요?
```

또 다른 방법은 Regular Expressions(정규식)을 사용하는 것입니다. `preg_replace()` 함수를 사용하여 텍스트를 검색하고 대체할 수 있습니다. 예를 들어, 다음과 같은 코드를 사용할 수 있습니다.
```PHP
  $text = "오늘은 날씨가 맑고 햇살이 참 좋습니다.";
  echo preg_replace("/맑고 (.*) 참/", "추위가 \1 지루해", $text);
```

위의 코드는 "맑고"와 "참" 사이에 있는 어떤 단어에 대해서도 대체를 수행합니다. 따라서 아래의 출력을 볼 수 있습니다.
```
오늘은 날씨가 추위가 지루해 햇살이 참 좋습니다.
```

## 심층적으로 들어가기
텍스트를 검색하고 대체하는 것은 간단한 작업처럼 보이지만, 정규식을 이해하고 다루는 것은 중요합니다. `()`를 사용하여 그룹으로 나누고 이후에 `\숫자`를 사용하여 순서대로 그룹을 참조할 수 있습니다.

또한, 정규식에서는 `.`, `?`, `+`와 같은 특수 문자의 의미가 있으므로 주의해야 합니다. 또한, 대체할 텍스트에서 `(`나 `\`를 사용한다면 백슬래시(`\`)를 추가하여 escaping(이스케이핑)해야 합니다.

더 자세한 정보는 PHP 공식 문서를 참조하시기 바랍니다.

# 또 다른 정보
- PHP `str_replace()` 함수 공식 문서: https://www.php.net/manual/en/function.str-replace.php
- PHP `preg_replace()` 함수 공식 문서: https://www.php.net/manual/en/function.preg-replace.php
- 정규식 테스트 사이트: https://regex101.com/