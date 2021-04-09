;; filename   : mql-mode.el
;; created at : 06 Apr 2021 16:56:11 JST
;; author     : hidsh
;;
;; This file is fully based on mql-mode.el created by jianingy. Thanks!
;; https://github.com/jianingy/emacs-ysl/blob/master/site-lisp/extra/mql-mode.el
;;
;; Example settings:
;; (use-package mql-mode
;;   :mode (("\\.mq4$" . mql-mode)
;;          ("\\.mqh$" . mql-mode))
;;   :config
;;   (setq mq4-compiler "C:/Users/XXXX/AppData/Roaming/MetaQuotes/WebInstall/mt4clw/metaeditor.exe")
;;   (add-hook 'mql-mode-hook (lambda () (flymake-mode t))))
;;
;;
;; TODO:
;; - support mql5
;; - support emacs27
;; - add something needs to mql-mode-keywords
;; - add something needs to mql-source-canidates
;; - packaging
;;

(require 'flymake)

(defvar mql-mode-exts '("\\.mq4\\'" "\\.mq5\\'" "\\.mqh\\'"))

(defvar mql-mode-keywords
  '(("\\<\\(?:Ask\\|B\\(?:ars\\|id\\)\\|Close\\|Digits\\|High\\|Low\\|Open\\|Point\\|\\(?:Ti\\|Volu\\)me\\)\\>" . font-lock-builtin-face)
    ("\\<\\(?:A\\(?:ccount\\(?:Balance\\|C\\(?:ompany\\|redit\\|urrency\\)\\|Equity\\|FreeMargin\\(?:Check\\|Mode\\)?\\|Leverage\\|Margin\\|N\\(?:ame\\|umber\\)\\|Profit\\|S\\(?:erver\\|topout\\(?:Level\\|Mode\\)\\)\\)\\|lert\\|rray\\(?:Bsearch\\|Copy\\(?:\\(?:Rat\\|Seri\\)es\\)?\\|Dimension\\|GetAsSeries\\|I\\(?:nitialize\\|sSeries\\)\\|M\\(?:\\(?:ax\\|in\\)imum\\)\\|R\\(?:\\(?:ang\\|esiz\\)e\\)\\|S\\(?:etAsSeries\\|ize\\|ort\\)\\)\\)\\|C\\(?:harToStr\\|omment\\)\\|D\\(?:ay\\(?:DayOfWeek\\|OfYear\\)\\|oubleToStr\\)\\|File\\(?:Close\\|Delete\\|Flush\\|Is\\(?:\\(?:Line\\)?Ending\\)\\|Open\\(?:History\\)?\\|Read\\(?:Array\\|Double\\|Integer\\|Number\\|String\\)\\|S\\(?:eek\\|ize\\)\\|Tell\\|Write\\(?:Array\\|Double\\|Integer\\|String\\)?\\)\\|G\\(?:et\\(?:LastError\\|TickCount\\)\\|lobalVariable\\(?:Check\\|Del\\|Get\\|Name\\|Set\\(?:OnCondition\\)?\\|s\\(?:\\(?:DeleteAl\\|Tota\\)l\\)\\)\\)\\|H\\(?:ideTestIndicators\\|our\\)\\|I\\(?:ndicator\\(?:Buffers\\|Counted\\|Digits\\|ShortName\\)\\|s\\(?:Connected\\|D\\(?:emo\\|llsAllowed\\)\\|ExpertEnabled\\|LibrariesAllowed\\|Optimization\\|Stopped\\|T\\(?:esting\\|rade\\(?:Allowed\\|ContextBusy\\)\\)\\|VisualMode\\)\\)\\|M\\(?:a\\(?:rketInfo\\|th\\(?:A\\(?:bs\\|rc\\(?:cos\\|\\(?:si\\|ta\\)n\\)\\)\\|C\\(?:eil\\|os\\)\\|Exp\\|Floor\\|Log\\|M\\(?:ax\\|in\\|od\\)\\|Pow\\|R\\(?:\\(?:a\\|ou\\)nd\\)\\|S\\(?:in\\|qrt\\|rand\\)\\|Tan\\)\\)\\|essageBox\\|inute\\|onth\\)\\|NormalizeDouble\\|O\\(?:bject\\(?:Create\\|De\\(?:lete\\|scription\\)\\|Find\\|Get\\(?:FiboDescription\\|ShiftByValue\\|ValueByShift\\)?\\|Move\\|Name\\|Set\\(?:FiboDescription\\|Text\\)?\\|Type\\|s\\(?:\\(?:DeleteAl\\|Tota\\)l\\)\\)\\|rder\\(?:C\\(?:lose\\(?:By\\|\\(?:Pric\\|Tim\\)e\\)?\\|omm\\(?:ent\\|ission\\)\\)\\|Delete\\|Expiration\\|Lots\\|M\\(?:agicNumber\\|odify\\)\\|Open\\(?:\\(?:Pric\\|Tim\\)e\\)\\|Pr\\(?:\\(?:in\\|ofi\\)t\\)\\|S\\(?:e\\(?:lect\\|nd\\)\\|topLoss\\|wap\\|ymbol\\)\\|T\\(?:akeProfit\\|icket\\|ype\\)\\|s\\(?:\\(?:History\\)?Total\\)\\)\\)\\|P\\(?:eriod\\|laySound\\|rint\\)\\|RefreshRates\\|S\\(?:e\\(?:conds\\|nd\\(?:FTP\\|Mail\\)\\|t\\(?:Index\\(?:Arrow\\|Buffer\\|DrawBegin\\|EmptyValue\\|Label\\|S\\(?:hift\\|tyle\\)\\)\\|Level\\(?:\\(?:Styl\\|Valu\\)e\\)\\)\\)\\|leep\\|tr\\(?:To\\(?:Double\\|Integer\\|Time\\)\\|ing\\(?:Concatenate\\|Find\\|GetChar\\|Len\\|S\\(?:\\(?:etCha\\|ubst\\)r\\)\\|Trim\\(?:\\(?:Lef\\|Righ\\)t\\)\\)\\)\\|ymbol\\)\\|T\\(?:erminal\\(?:Company\\|Name\\|Path\\)\\|ime\\(?:Current\\|Day\\(?:Of\\(?:Week\\|Year\\)\\)?\\|Hour\\|Local\\|M\\(?:inute\\|onth\\)\\|Seconds\\|\\(?:ToSt\\|Yea\\)r\\)\\)\\|UninitializeReason\\|Window\\(?:BarsPerChart\\|ExpertName\\|Fi\\(?:nd\\|rstVisibleBar\\)\\|Handle\\|IsVisible\\|OnDropped\\|Price\\(?:M\\(?:ax\\|in\\)\\|OnDropped\\)\\|Redraw\\|ScreenShot\\|TimeOnDropped\\|XOnDropped\\|YOnDropped\\|sTotal\\)\\|Year\\|i\\(?:A\\(?:DX\\|TR\\|lligator\\|[CDO]\\)\\|B\\(?:WMFI\\|a\\(?:nds\\(?:OnArray\\)?\\|r\\(?:Shift\\|s\\)\\)\\|\\(?:ear\\|ull\\)sPower\\)\\|C\\(?:CI\\(?:OnArray\\)?\\|lose\\|ustom\\)\\|DeMarker\\|Envelopes\\(?:OnArray\\)?\\|F\\(?:orce\\|ractals\\)\\|Gator\\|High\\(?:est\\)?\\|Ichimoku\\|Low\\(?:est\\)?\\|M\\(?:A\\(?:CD\\|OnArray\\)?\\|FI\\|omentum\\(?:OnArray\\)?\\)\\|O\\(?:BV\\|pen\\|sMA\\)\\|R\\(?:SI\\(?:OnArray\\)?\\|VI\\)\\|S\\(?:AR\\|t\\(?:dDev\\(?:OnArray\\)?\\|ochastic\\)\\)\\|Time\\|Volume\\|WPR\\)\\)\\>" . font-lock-function-name-face)))

(defun mql-source-canidates ()
  '("AccountBalance" "AccountCredit"  "AccountCompany"  "AccountCurrency"  "AccountEquity"  "AccountFreeMargin"  "AccountFreeMarginCheck"  "AccountFreeMarginMode"  "AccountLeverage"  "AccountMargin"  "AccountName"  "AccountNumber"  "AccountProfit"  "AccountServer"  "AccountStopoutLevel"  "AccountStopoutMode"  "Alert"  "ArrayBsearch"  "ArrayCopy"  "ArrayCopyRates"  "ArrayCopySeries"  "ArrayDimension"  "ArrayGetAsSeries"  "ArrayInitialize"  "ArrayIsSeries"  "ArrayMaximum"  "ArrayMinimum"  "ArrayRange"  "ArrayResize"  "ArraySetAsSeries"  "ArraySize"  "ArraySort"  "CharToStr"  "Comment"  "DayDayOfWeek"  "DayOfYear"  "DoubleToStr"  "FileClose"  "FileDelete"  "FileFlush"  "FileIsEnding"  "FileIsLineEnding"  "FileOpen"  "FileOpenHistory"  "FileReadArray"  "FileReadDouble"  "FileReadInteger"  "FileReadNumber"  "FileReadString"  "FileSeek"  "FileSize"  "FileTell"  "FileWrite"  "FileWriteArray"  "FileWriteDouble"  "FileWriteInteger"  "FileWriteString"  "GetLastError"  "GetTickCount"  "GlobalVariableCheck"  "GlobalVariableDel"  "GlobalVariableGet"  "GlobalVariableName"  "GlobalVariableSet"  "GlobalVariableSetOnCondition"  "GlobalVariablesDeleteAll"  "GlobalVariablesTotal"  "HideTestIndicators"  "Hour"  "iAC"  "iAD"  "iAlligator"  "iADX"  "iATR"  "iAO"  "iBars"  "iBarShift"  "iBearsPower"  "iBands"  "iBandsOnArray"  "iBullsPower"  "iBWMFI"  "iCCI"  "iCCIOnArray"  "iClose"  "iCustom"  "iDeMarker"  "iEnvelopes"  "iEnvelopesOnArray"  "iForce"  "iFractals"  "iGator"  "iIchimoku"  "iMomentum"  "iMomentumOnArray"  "iMFI"  "iMA"  "iMACD"  "iMAOnArray"  "iHigh"  "iHighest"  "iLow"  "iLowest"  "iOBV"  "iOpen"  "iOsMA"  "IndicatorBuffers"  "IndicatorCounted"  "IndicatorDigits"  "IndicatorShortName"  "iRSI"  "iRSIOnArray"  "iRVI"  "iSAR"  "IsConnected"  "IsDemo"  "IsDllsAllowed"  "IsExpertEnabled"  "IsLibrariesAllowed"  "IsOptimization"  "IsStopped"  "iStdDev"  "iStdDevOnArray"  "iStochastic"  "IsTesting"  "IsTradeAllowed"  "IsTradeContextBusy"  "IsVisualMode"  "iTime"  "iVolume"  "iWPR"  "MarketInfo"  "MathAbs"  "MathArccos"  "MathArcsin"  "MathArctan"  "MathCeil"  "MathCos"  "MathExp"  "MathFloor"  "MathLog"  "MathMax"  "MathMin"  "MathMod"  "MathPow"  "MathRand"  "MathRound"  "MathSin"  "MathSqrt"  "MathSrand"  "MathTan"  "MessageBox"  "Minute"  "Month"  "NormalizeDouble"  "ObjectCreate"  "ObjectDelete"  "ObjectDescription"  "ObjectFind"  "ObjectGet"  "ObjectGetFiboDescription"  "ObjectGetShiftByValue"  "ObjectGetValueByShift"  "ObjectMove"  "ObjectName"  "ObjectsDeleteAll"  "ObjectSet"  "ObjectSetFiboDescription"  "ObjectSetText"  "ObjectsTotal"  "ObjectType"  "OrderClose"  "OrderCloseBy"  "OrderClosePrice"  "OrderCloseTime"  "OrderComment"  "OrderCommission"  "OrderDelete"  "OrderExpiration"  "OrderLots"  "OrderMagicNumber"  "OrderModify"  "OrderOpenPrice"  "OrderOpenTime"  "OrderPrint"  "OrderProfit"  "OrderSelect"  "OrderSend"  "OrdersHistoryTotal"  "OrderStopLoss"  "OrdersTotal"  "OrderSwap"  "OrderSymbol"  "OrderTakeProfit"  "OrderTicket"  "OrderType"  "Period"  "PlaySound"  "Print"  "RefreshRates"  "Seconds"  "SendFTP"  "SendMail"  "SetIndexArrow"  "SetIndexBuffer"  "SetIndexDrawBegin"  "SetIndexEmptyValue"  "SetIndexLabel"  "SetIndexShift"  "SetIndexStyle"  "SetLevelStyle"  "SetLevelValue"  "Sleep"  "StringConcatenate"  "StringFind"  "StringGetChar"  "StringLen"  "StringSetChar"  "StringSubstr"  "StringTrimLeft"  "StringTrimRight"  "StrToDouble"  "StrToInteger"  "StrToTime"  "Symbol"  "TerminalCompany"  "TerminalName"  "TerminalPath"  "TimeCurrent"  "TimeDay"  "TimeDayOfWeek"  "TimeDayOfYear"  "TimeHour"  "TimeLocal"  "TimeMinute"  "TimeMonth"  "TimeSeconds"  "TimeToStr"  "TimeYear"  "UninitializeReason"  "WindowBarsPerChart"  "WindowExpertName"  "WindowFind"  "WindowFirstVisibleBar"  "WindowHandle"  "WindowIsVisible"  "WindowOnDropped"  "WindowPriceMax"  "WindowPriceMin"  "WindowPriceOnDropped"  "WindowRedraw"  "WindowScreenShot"  "WindowTimeOnDropped"  "WindowsTotal"  "WindowXOnDropped"  "WindowYOnDropped"  "Year" "Ask" "Bars" "Bid" "Close" "Digits" "High" "Low" "Open" "Point" "Time" "Volume" "input"))

(defvar ac-source-mql
  '((candidates . mql-source-canidates)))

;; flymake and compile
(defcustom mq5-compiler "C:/SOMEWHERE/metaeditor.exe"
  "Path string of \"metaeditor.exe\" for MQL5")

(defcustom mq4-compiler "C:/SOMEWHERE/metaeditor.exe"
  "Path string of \"metaeditor.exe\" for MQL4")

(defvar mq4-bat ""
  "Path string of \"flymake-mq4.bat\"")

(defvar mql-mode-directory
  (if load-file-name
      (file-name-directory load-file-name) ;; File is being loaded.
    default-directory))                    ;; File is being evaluated using, for example, `eval-buffer'.

(defun mql-mode-get-mq4-path ()
  (let ((path buffer-file-name)
        (last-dir "MQL4"))
    (if path
        (substring path 0 (+ (string-match last-dir path) (length last-dir)))
      "")))

;;;###autoload
(defun flymake-mq4-init ()
  (when (string-empty-p mq4-bat)
    (setq mq4-bat (concat mql-mode-directory "/mq4.bat")))
  (unless (file-executable-p mq4-bat)
    (error "Not found %s" mq4-bat))
  (unless (file-exists-p mq4-compiler)
    (error "MQL4 compiler not found: %s" mq4-compiler))
  (let* ((temp-file   (flymake-proc-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-dir   (file-name-directory buffer-file-name))
         (local-file  (file-relative-name
                       temp-file
                       local-dir))
         (log-file    (concat (file-name-base local-file) ".log")))
    (list mq4-bat (list "flymake" local-dir mq4-compiler local-file (mql-mode-get-mq4-path) log-file))))

;;;###autoload
;; fixme
(defun flymake-mq5-init ()
  (interactive)
  ;; todo
  )

;;;###autoload
(defun compile-mq4 ()
  "Compile mql4 file"
  (interactive)
  (when (string-empty-p mq4-bat)
    (setq mq4-bat (concat mql-mode-directory "/mq4.bat")))
  (unless (file-executable-p mq4-bat)
    (error "Not found %s" mq4-bat))
  (unless buffer-file-name
    (error "This buffer does not have a file name"))
  (unless (file-executable-p mq4-compiler)
    (error "MQL4 compiler not found: %s" mq4-compiler))
  (let ((dir (file-name-directory buffer-file-name)))
    (compile (format "%s compile %s %s %s %s" mq4-bat dir mq4-compiler (buffer-file-name) (mql-mode-get-mq4-path))))
  (run-hooks 'mql-mode-compile-hook))

;;;###autoload
;; fixme
(defun compile-mq5 ()
  "Compile mql5 file."
  (interactive)
  ;; TODO
  )

;;;###autoload
(define-derived-mode mql-mode c++-mode "MQL"
  "Major mode for editing MetaTrader4/5 MQL file."
  (font-lock-add-keywords 'mql-mode mql-mode-keywords)
  (set (make-local-variable 'compilation-scroll-output) t)

  (dolist (ext mql-mode-exts)
    (modify-coding-system-alist 'file ext 'utf-8-dos))

  (cond ((string= (file-name-extension (buffer-file-name)) "mq4")
         (define-key mql-mode-map "\C-c\C-c" 'compile-mq4)
         (push '("\\.mq4$" flymake-mq4-init) flymake-proc-allowed-file-name-masks)
         (push '("^\\(.+\.mq4\\|.+\.mqh\\)(\\([0-9]+\\),\\([0-9]+\\)) : \\(.+\\)$"
                 1 2 3 4) flymake-proc-err-line-patterns))

        ((string= (file-name-extension (buffer-file-name)) "mq5")
         (define-key mql-mode-map "\C-c\C-c" 'compile-mq5)
         (push '("\\.mq5$" flymake-mq5-init) flymake-proc-allowed-file-name-masks)
         (push '("^\\(.+\.mq5\\|.+\.mqh\\)(\\([0-9]+\\),\\([0-9]+\\)) : \\(.+\\)$"
                 1 2 3 4) flymake-proc-err-line-patterns)))

  (define-key mql-mode-map "\C-c\C-p" 'flymake-goto-prev-error)
  (define-key mql-mode-map "\C-c\C-n" 'flymake-goto-next-error)
  (push '("\\.mqh$" flymake-mq4-init) flymake-proc-allowed-file-name-masks))

(defvar mql-mode-hook nil)
(add-hook 'mql-mode-hook (lambda () (setq c-basic-offset 3
                                          tab-width 3)))
(defvar mql-mode-compile-hook nil)

;;;###autoload
(dolist (ext mql-mode-exts)
  (add-to-list 'auto-mode-alist (cons ext 'mql-mode) t))

(provide 'mql-mode)
