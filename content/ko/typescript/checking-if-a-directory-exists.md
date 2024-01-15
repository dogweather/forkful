---
title:                "디렉토리 존재 여부 확인하기"
html_title:           "TypeScript: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것의 중요성을 반드시 이해해야 합니다. 이를 통해 파일 시스템 내에서 지정된 경로에 파일이나 폴더가 존재하는지 여부를 알 수 있습니다.

## 방법

```TypeScript
if (fs.existsSync("/경로/디렉토리")) {
  console.log("디렉토리가 존재합니다.");
} else {
  console.log("디렉토리가 존재하지 않습니다.");
}
```

위의 코드에서, 우리는 `fs.existsSync()` 함수를 사용하여 디렉토리가 존재하는지 확인할 수 있습니다. 만약 디렉토리가 존재한다면 `true`를 반환하고, 그렇지 않다면 `false`를 반환합니다.

```TypeScript
fs.access("/경로/디렉토리",(err) => {
  if (err) {
    console.log("디렉토리가 존재하지 않습니다.");
  } else {
    console.log("디렉토리가 존재합니다.");
  }
});
```

또 다른 방법으로는 `fs.access()` 함수를 사용하는 것입니다. 이 함수는 파일이나 폴더에 액세스할 수 있는지 확인합니다. 만약 액세스할 수 없다면 에러가 발생하고, 이를 통해 디렉토리가 존재하지 않는지를 알 수 있습니다.

## 깊게 들어가보기

위에서 언급한 두 가지 방법 외에도, `fs.lstat()` 함수를 사용하는 방법도 있습니다. 이 함수는 디렉토리가 아닌 파일일 경우 에러를 반환합니다. 이를 통해 디렉토리와 파일을 분류하는 것이 가능합니다.

또한, 옵션 객체를 함께 사용하여 디렉토리의 존재 여부 뿐만 아니라 디렉토리에 대한 다양한 정보를 얻을 수도 있습니다.

## 관련 자료

- [Node.js 공식 문서 - fs 모듈](https://nodejs.org/api/fs.html)
- [Node.js Tutorial - 파일 시스템 다루기](https://www.tutorialspoint.com/nodejs/nodejs_file_system.htm)
- [NPM 라이브러리 - fs-extra](https://www.npmjs.com/package/fs-extra)