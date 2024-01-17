---
title:                "ウェブページをダウンロードする"
html_title:           "C: ウェブページをダウンロードする"
simple_title:         "ウェブページをダウンロードする"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何となく?
ウェブページをダウンロードすることとは、ウェブ上にある特定のページの内容を保存することです。開発者がこれを行う理由は、ウェブページの情報をより便利にアクセスできるようにするためです。

## 方法:
```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
	CURL *curl;
	CURLcode res;
	FILE *fp;
	char *url = "https://www.example.com";
	char outfilename[FILENAME_MAX] = "example.html";
	curl = curl_easy_init();
	if (curl)
	{
		fp = fopen(outfilename,"wb");
		curl_easy_setopt(curl, CURLOPT_URL, url);
		curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
		curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
		res = curl_easy_perform(curl);
		curl_easy_cleanup(curl);
		fclose(fp);
	}
	return 0;
}
```
```
$ gcc example.c -lcurl
$ ./a.out
```
example.htmlファイルが生成され、その中にウェブページの内容が保存されます。

## もっと深く:
ウェブページのダウンロードは、昔から使われているシンプルで便利な手段です。代替方法としては、サービスやツールを使用することもできますが、直接ダウンロードする方がよりカスタマイズ可能です。ウェブページのダウンロードには、cURLを使う方法以外にも多数の手段がありますが、今回はcURLを使用しました。また、HTTPやFTPなど、様々なプロトコルを使用してウェブページをダウンロードすることができます。

## 詳しくは:
- [cURL公式ドキュメント] (https://curl.haxx.se/libcurl/c/)
- [cURLコード例] (https://curl.haxx.se/libcurl/c/example.html)
- [ウェブページのダウンロード方法] (https://www.developerdrive.com/download-web-page-using-curl/)