ξ ^ω^)ξ ＜ Twittering TwinDrill!
===================================

`TwinDrill-mode`はEmacs用のTwitterクライアントだよ。[hayamiz/twittering-mode](https://github.com/hayamiz/twittering-mode)から派生しますた。

- https://github.com/zonuexe/twindrill-mode

機能
----

* Twitterの機能への対応状況:
  * 様々なタイムラインの閲覧
    * フレンドタイムライン
    * 返信
    * ユーザーのタイムライン
    * パブリックタイムライン
  * つぶやきの投稿
    * ダイレクトメッセージ
    * ReTweet
    * ハッシュタグ
    * 署名
  * フォロー/リムーブ
  * お気に入り
* HTTPプロキシ対応
* HTTPSによる安全な通信

### twittering-modeからの変更点

* Emacs 21向けのpolyfillを削除
* win-curlのバンドルを削除
* twittering-jojo-modeを削除
* *(未実装)* [chumpage/mag-menu](https://github.com/chumpage/mag-menu)を使ったUI

動作確認環境
------------

- GNU Emacs 24

作者と協力者
------------

- Y. Hayamizu
- naoya_t
- Tsuyoshi CHO
- Alberto Garcia
- Satoshi Yatagawa
- 高山智也
- 松尾(cvmat)
- 青田(naota)
- Jaemok Jeong(jmjeong)
- Thomas Danckaert
- IMAI Toshiyuki

参考
----

- http://www.emacswiki.org/emacs-en/TwitteringMode
