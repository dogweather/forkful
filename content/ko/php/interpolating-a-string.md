---
title:                "문자열 보간"
html_title:           "PHP: 문자열 보간"
simple_title:         "문자열 보간"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

스트링을 interpolate 한다는 것은 무엇일까요? 쉽게 말하면, 우리가 보통 문자열을 나타내는 작은 따옴표(')나 큰 따옴표(") 대신에 변수나 함수를 포함할 수 있도록 만드는 것입니다. 이렇게 하면 변수나 함수를 문자열 내에 직접 삽입할 수 있어서 변수나 함수를 가지고 작업하는 데 더 편리하게 됩니다. 이것이 왜 프로그래머들이 interpolate를 사용하는지 이유입니다.

# 하는 방법:

Php 언어로 interpolate를 하는 방법은 매우 간단합니다. 우선, 이 글이 적히고 있는 이 부분도 Php로 인터프리트되고 있으니 따옴표 안에 변수나 함수를 넣고 싶다면 `<?php` 와 `?>` 태그 중간에 `interpolate` 함수를 사용하시면 됩니다. 그리고 작은 따옴표 대신 큰 따옴표를 사용하면 됩니다. 아래 예시를 보시죠.

```PHP
<?php
    $name = "John";
    echo "내 이름은 {$name}입니다."; 
    //=> "내 이름은 John입니다."
?>
```

이렇게 코드를 실행하면 `{}` 안에 있는 변수나 함수의 값을 참조해서 문자열을 출력하기 때문에 문자열과 변수나 함수를 함께 사용할 수 있게 됩니다.

당연한 얘기지만, 작은 따옴표를 사용하면 변수나 함수를 인식하지 못하고 그냥 문자열로 출력되니 조심하세요. 

```PHP
<?php
    $name = "John";
    echo '내 이름은 {$name}입니다.'; 
    //=> "내 이름은 {$name}입니다."
?>
```

# 깊게 파보기:

추가적인 정보를 알고싶다면, 확장된 따옴표(interpolated string)는 C언어에서 부터 시작되었습니다. Javascript의 template strings의 문법과도 유사한데, 만약 자세한 정보를 알고싶다면 해당 언어들의 문서를 참조하시면 도움될 것입니다. 또한, Php에서도 `sprintf()` 함수를 사용하는 것이 가능합니다. 하지만 이 함수는 변수나 함수가 아닌, 문자열에 값이 들어갈 위치를 지정하는 것에 널리 사용되지만 보통 문자열을 결합하고 변환하는 데에는 인터폴레이트나 템플릿 문자열을 사용하는 것이 보다 간단하고 효율적입니다.

# 더 알아보기:

- [PHP Official Documentation](https://www.php.net/manual/kr/language.types.string.php)
- [Template Literals - JavaScript](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Template_literals)