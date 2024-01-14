---
title:    "PHP: 텍스트 찾기와 바꾸기"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## 왜

텍스트를 찾고 대체하는 것에 대해 이야기하는 이유는, 우리가 개발자로서 일하는 동안 어떤 상황에서 이 기술이 유용할지에 대해 이해하기 위해서 입니다.

## 어떻게

텍스트를 찾고 대체하는 방법은 여러 가지가 있습니다. 여러분은 PHP의 `str_replace()` 함수를 사용하여 이 작업을 간단하게 할 수 있습니다. 다음은 이 함수를 사용하는 간단한 예제입니다.

```PHP
$message = "안녕하세요, PHP 개발자 여러분!";
$new_message = str_replace("PHP", "JavaScript", $message);
echo $new_message;
```
출력 결과는 다음과 같습니다.

```
안녕하세요, JavaScript 개발자 여러분!
```

위 예제에서 볼 수 있듯이, `str_replace()` 함수는 세 개의 매개변수를 가집니다. 첫 번째 매개변수는 찾고 싶은 단어나 문자열이고, 두 번째 매개변수는 대체할 단어나 문자열이며, 세 번째 매개변수는 원본 문자열입니다.

또 다른 유용한 함수는 `preg_replace()` 함수입니다. 이 함수는 정규식을 사용하여 좀 더 복잡하고 구체적인 패턴을 찾고 대체할 수 있습니다. 예를 들어, 다음은 이 함수를 사용하여 이메일 주소 중에서 도메인 이름만 찾아 그 값을 `example.com`으로 대체하는 예제입니다.

```PHP
$email = "example@gmail.com";
$new_email = preg_replace("/(\w+)@(.+)\.com/", "$1@example.com", $email);
echo $new_email;
```
출력 결과는 다음과 같습니다.

```
example@example.com
```

## 깊이 파헤치기

텍스트를 찾고 대체하는 것은 쉽지만, 정확한 패턴을 사용하여 원하는 대상을 찾아내는 것은 어려울 수 있습니다. 따라서 정규식을 학습하는 것은 매우 중요합니다. 정규식은 일정한 패턴에 일치하는 문자열을 찾기 위해 사용하는 표현식입니다. 이를 통해 복잡한 문자열을 빠르고 유연하게 처리할 수 있습니다.

또 다른 중요한 요소는 대소문자를 구분하는지 여부입니다. 기본값으로는 대소문자를 구분하지 않는데, 이는 대부분의 상황에서는 원하는 결과를 얻을 수 있지만, 때로는 정확한 검색을 위해 대소문자 구분 설정을 변경해야 할 수도 있습니다.

## 더 알아보기

- [PHP 공식 문서 - str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [PHP 공식 문서 - preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [정규식 테스트 사이트](https://regexr.com/)