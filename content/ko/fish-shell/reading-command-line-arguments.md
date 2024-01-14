---
title:    "Fish Shell: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 왜

혹시 커맨드 라인 인자를 읽어들이는 것에 대해 궁금해 하신적이 있으신가요? 커맨드 라인 인자는 프로그래밍에서 꽤 중요한 역할을 합니다. 이 블로그 포스트에서는 다른 저장소나 프로그램에서 필요한 정보를 얻는데 사용하는 강력한 기능으로, 커맨드 라인 인자를 읽는 방법에 대해서 알아보려고 합니다.

## 어떻게

커맨드 라인 인자를 읽는 방법은 매우 간단합니다. 먼저 다음과 같은 형태로 실행하면 됩니다:

```Fish Shell
$ fish script.fish argument1 argument2 argument3
```

위의 예시에서 `script.fish`는 스크립트 파일의 이름이며, `argument1`, `argument2`, `argument3`은 각각 인자(argument)라고 부릅니다. 스크립트 파일에서 인자들을 읽는 방법은 다양하지만, 가장 일반적인 방법은 `$argv` 변수를 사용하는 것입니다. 이 변수는 현재 스크립트로 전달된 모든 인자들을 배열로 저장해 줍니다. 예를 들어, 만약 `script.fish`에서 `$argv`를 이용해서 첫번째 인자를 출력하고 싶다면 다음과 같이 하면 됩니다:

```Fish Shell
echo $argv[1]
```

위의 예시에서는 `argument1`이 출력될 것입니다. 마찬가지로 `argument2`를 출력하려면 `$argv[2]`를 이용하시면 됩니다.

## 딥 다이브

커맨드 라인 인자는 다양한 프로그래밍 케이스에서 유용하게 사용됩니다. 예를 들어, 저장소를 다운로드 받을 때 인자를 이용해서 저장소의 주소를 설정할 수 있습니다. 또한, 프로그램을 실행할 때 필요한 설정 값을 인자로 전달할 수도 있습니다. 더 전문적인 내용은 다음 링크를 참고해주세요:

* [Fish Shell 공식 문서](https://fishshell.com/docs/current/scripting.html#using-command-line-arguments)
* [Fish Shell 스크립트 예제](https://github.com/fish-shell/fish-shell/wiki/Scripting-examples#scripting-fish)

## See Also

* [Fish Shell 공식 홈페이지](https://fishshell.com/)
* [Fish Shell 한국어 위키](https://github.com/fish-shell/fish-shell/wiki/%ED%8C%8C%EC%9D%BC%ED%88%B4-%EC%9C%A0%ED%94%BD-%ED%95%9C%EA%B5%AD%EC%96%B4-%ED%99%95%EC%9D%B8)