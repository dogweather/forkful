---
title:    "PHP: 패턴과 일치하는 문자 삭제하기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

캐릭터 패턴매칭을 삭제하는 것의 장점은 굉장히 다양합니다. 이를 통해 코드를 더 깔끔하고 효율적으로 만들 수 있으며, 추후에 수정이나 유지보수를 더욱 쉽게 할 수 있습니다.

## 방법

캐릭터 패턴매칭을 삭제하는 방법은 굉장히 간단합니다. ```preg_replace()``` 함수를 사용하여 해당 패턴을 찾고, 삭제하고자하는 문자열을 빈 문자열로 대체하면 됩니다.

먼저, 삭제하고자하는 특정한 패턴을 설정해줍니다. 예를 들어, a부터 z까지의 소문자를 모두 삭제하고 싶다면 아래와 같이 정규표현식을 작성합니다.

```PHP
$pattern = '/[a-z]/';
```

그리고 이를 ```preg_replace()``` 함수에 적용합니다.

```PHP
$result = preg_replace($pattern, '', $string);
```

마지막으로, 실제 문자열을 출력해보면 설정한 패턴에 해당하는 문자가 모두 삭제된 것을 확인할 수 있습니다.

```PHP
echo $result;
// 원래 문자열: "안녕하세요, PHP 프로그래밍"
// 출력 결과: "안녕하세요프로그래밍"
```

## 딥 다이브

캐릭터 패턴매칭을 삭제하는 기능은 정규표현식 사용법을 알고 있다면 더욱 쉽게 사용할 수 있습니다. 정규표현식을 사용하여 더 복잡한 패턴을 지정할 수 있고, 옵션을 설정하여 대소문자를 구분할지 여부 등을 결정할 수 있습니다.

또한, ```preg_replace()``` 함수 이외에도 ```substr()``` 함수를 사용하여 문자열의 특정 부분을 삭제하는 방법도 있습니다. 이는 정규표현식에 익숙하지 않은 사용자에게 더 쉬운 대안이 될 수 있습니다.

## 더 알아보기

- PHP 정규표현식 사용법: https://www.php.net/manual/kr/function.preg-replace.php
- PHP 문자열 함수: https://www.php.net/manual/kr/ref.strings.php
- 정규표현식 기초: http://www.nextree.co.kr/p4327/