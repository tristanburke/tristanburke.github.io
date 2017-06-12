function init_daff() {
  var exports = {};
  function require(x) {
    return {};
  }
  var module = 1;
	(function (console, $hx_exports) { "use strict";
	$hx_exports.coopy = $hx_exports.coopy || {};
	var HxOverrides = function() { };
	HxOverrides.__name__ = true;
	HxOverrides.dateStr = function(date) {
		var m = date.getMonth() + 1;
		var d = date.getDate();
		var h = date.getHours();
		var mi = date.getMinutes();
		var s = date.getSeconds();
		return date.getFullYear() + "-" + (m < 10?"0" + m:"" + m) + "-" + (d < 10?"0" + d:"" + d) + " " + (h < 10?"0" + h:"" + h) + ":" + (mi < 10?"0" + mi:"" + mi) + ":" + (s < 10?"0" + s:"" + s);
	};
	HxOverrides.cca = function(s,index) {
		var x = s.charCodeAt(index);
		if(x != x) return undefined;
		return x;
	};
	HxOverrides.substr = function(s,pos,len) {
		if(pos != null && pos != 0 && len != null && len < 0) return "";
		if(len == null) len = s.length;
		if(pos < 0) {
			pos = s.length + pos;
			if(pos < 0) pos = 0;
		} else if(len < 0) len = s.length + len - pos;
		return s.substr(pos,len);
	};
	HxOverrides.iter = function(a) {
		return { cur : 0, arr : a, hasNext : function() {
			return this.cur < this.arr.length;
		}, next : function() {
			return this.arr[this.cur++];
		}};
	};
	var Lambda = function() { };
	Lambda.__name__ = true;
	Lambda.array = function(it) {
		var a = [];
		var $it0 = $iterator(it)();
		while( $it0.hasNext() ) {
			var i = $it0.next();
			a.push(i);
		}
		return a;
	};
	Lambda.map = function(it,f) {
		var l = new List();
		var $it0 = $iterator(it)();
		while( $it0.hasNext() ) {
			var x = $it0.next();
			l.add(f(x));
		}
		return l;
	};
	Lambda.has = function(it,elt) {
		var $it0 = $iterator(it)();
		while( $it0.hasNext() ) {
			var x = $it0.next();
			if(x == elt) return true;
		}
		return false;
	};
	var List = function() {
		this.length = 0;
	};
	List.__name__ = true;
	List.prototype = {
		add: function(item) {
			var x = [item];
			if(this.h == null) this.h = x; else this.q[1] = x;
			this.q = x;
			this.length++;
		}
		,iterator: function() {
			return new _List.ListIterator(this.h);
		}
		,__class__: List
	};
	var _List = {};
	_List.ListIterator = function(head) {
		this.head = head;
		this.val = null;
	};
	_List.ListIterator.__name__ = true;
	_List.ListIterator.prototype = {
		hasNext: function() {
			return this.head != null;
		}
		,next: function() {
			this.val = this.head[0];
			this.head = this.head[1];
			return this.val;
		}
		,__class__: _List.ListIterator
	};
	var Reflect = function() { };
	Reflect.Math.__name__ = true;
	__name__ = true;
	Reflect.field = function(o,field) {
		try {
			return o[field];
		} catch( e ) {
			return null;
		}
	};
	Reflect.setField = function(o,field,value) {
		o[field] = value;
	};
	Reflect.fields = function(o) {
		var a = [];
		if(o != null) {
			var hasOwnProperty = Object.prototype.hasOwnProperty;
			for( var f in o ) {
			if(f != "__id__" && f != "hx__closures__" && hasOwnProperty.call(o,f)) a.push(f);
			}
		}
		return a;
	};
	Reflect.isFunction = function(f) {
		return typeof(f) == "function" && !(f.__name__ || f.__ename__);
	};
	Reflect.compare = function(a,b) {
		if(a == b) return 0; else if(a > b) return 1; else return -1;
	};
	var Std = function() { };
	Std.__name__ = true;
	Std.string = function(s) {
		return js.Boot.__string_rec(s,"");
	};
	Std.parseInt = function(x) {
		var v = parseInt(x,10);
		if(v == 0 && (HxOverrides.cca(x,1) == 120 || HxOverrides.cca(x,1) == 88)) v = parseInt(x);
		if(isNaN(v)) return null;
		return v;
	};
	Std.parseFloat = function(x) {
		return parseFloat(x);
	};
	var StringBuf = function() {
		this.b = "";
	};
	StringBuf.__name__ = true;
	StringBuf.prototype = {
		__class__: StringBuf
	};
	var StringTools = function() { };
	StringTools.__name__ = true;
	StringTools.isSpace = function(s,pos) {
		var c = HxOverrides.cca(s,pos);
		return c > 8 && c < 14 || c == 32;
	};
	StringTools.ltrim = function(s) {
		var l = s.length;
		var r = 0;
		while(r < l && StringTools.isSpace(s,r)) r++;
		if(r > 0) return HxOverrides.substr(s,r,l - r); else return s;
	};
	StringTools.rtrim = function(s) {
		var l = s.length;
		var r = 0;
		while(r < l && StringTools.isSpace(s,l - r - 1)) r++;
		if(r > 0) return HxOverrides.substr(s,0,l - r); else return s;
	};
	StringTools.trim = function(s) {
		return StringTools.ltrim(StringTools.rtrim(s));
	};
	StringTools.lpad = function(s,c,l) {
		if(c.length <= 0) return s;
		while(s.length < l) s = c + s;
		return s;
	};
	StringTools.replace = function(s,sub,by) {
		return s.split(sub).join(by);
	};
	StringTools.fastCodeAt = function(s,index) {
		return s.charCodeAt(index);
	};
	var ValueType = { __ename__ : true, __constructs__ : ["TNull","TInt","TFloat","TBool","TObject","TFunction","TClass","TEnum","TUnknown"] };
	ValueType.TNull = ["TNull",0];
	ValueType.TNull.__enum__ = ValueType;
	ValueType.TInt = ["TInt",1];
	ValueType.TInt.__enum__ = ValueType;
	ValueType.TFloat = ["TFloat",2];
	ValueType.TFloat.__enum__ = ValueType;
	ValueType.TBool = ["TBool",3];
	ValueType.TBool.__enum__ = ValueType;
	ValueType.TObject = ["TObject",4];
	ValueType.TObject.__enum__ = ValueType;
	ValueType.TFunction = ["TFunction",5];
	ValueType.TFunction.__enum__ = ValueType;
	ValueType.TClass = function(c) { var $x = ["TClass",6,c]; $x.__enum__ = ValueType; return $x; };
	ValueType.TEnum = function(e) { var $x = ["TEnum",7,e]; $x.__enum__ = ValueType; return $x; };
	ValueType.TUnknown = ["TUnknown",8];
	ValueType.TUnknown.__enum__ = ValueType;
	var Type = function() { };
	Type.__name__ = true;
	Type["typeof"] = function(v) {
		var _g = typeof(v);
		switch(_g) {
		case "boolean":
			return ValueType.TBool;
		case "string":
			return ValueType.TClass(String);
		case "number":
			if(Math.ceil(v) == v % 2147483648.0) return ValueType.TInt;
			return ValueType.TFloat;
		case "object":
			if(v == null) return ValueType.TNull;
			var e = v.__enum__;
			if(e != null) return ValueType.TEnum(e);
			var c = js.Boot.getClass(v);
			if(c != null) return ValueType.TClass(c);
			return ValueType.TObject;
		case "function":
			if(v.__name__ || v.__ename__) return ValueType.TObject;
			return ValueType.TFunction;
		case "undefined":
			return ValueType.TNull;
		default:
			return ValueType.TUnknown;
		}
	};
	Type.enumIndex = function(e) {
		return e[1];
	};
	var coopy = {};
	coopy.Alignment = function() {
		this.map_a2b = new haxe.ds.IntMap();
		this.map_b2a = new haxe.ds.IntMap();
		this.ha = this.hb = 0;
		this.map_count = 0;
		this.reference = null;
		this.meta = null;
		this.comp = null;
		this.order_cache_has_reference = false;
		this.ia = -1;
		this.ib = -1;
		this.marked_as_identical = false;
	};
	coopy.Alignment.__name__ = true;
	coopy.Alignment.prototype = {
		range: function(ha,hb) {
			this.ha = ha;
			this.hb = hb;
		}
		,tables: function(ta,tb) {
			this.ta = ta;
			this.tb = tb;
		}
		,headers: function(ia,ib) {
			this.ia = ia;
			this.ib = ib;
		}
		,setRowlike: function(flag) {
		}
		,link: function(a,b) {
			if(a != -1) this.map_a2b.h[a] = b; else this.has_addition = true;
			if(b != -1) this.map_b2a.h[b] = a; else this.has_removal = true;
			this.map_count++;
		}
		,addIndexColumns: function(unit) {
			if(this.index_columns == null) this.index_columns = [];
			this.index_columns.push(unit);
		}
		,getIndexColumns: function() {
			return this.index_columns;
		}
		,a2b: function(a) {
			return this.map_a2b.h[a];
		}
		,b2a: function(b) {
			return this.map_b2a.h[b];
		}
		,count: function() {
			return this.map_count;
		}
		,toString: function() {
			var result = "" + this.map_a2b.toString() + " // " + this.map_b2a.toString();
			if(this.reference != null) result += " (" + Std.string(this.reference) + ")";
			return result;
		}
		,toOrder: function() {
			if(this.order_cache != null) {
				if(this.reference != null) {
					if(!this.order_cache_has_reference) this.order_cache = null;
				}
			}
			if(this.order_cache == null) this.order_cache = this.toOrder3();
			if(this.reference != null) this.order_cache_has_reference = true;
			return this.order_cache;
		}
		,addToOrder: function(l,r,p) {
			if(p == null) p = -2;
			if(this.order_cache == null) this.order_cache = new coopy.Ordering();
			this.order_cache.add(l,r,p);
			this.order_cache_has_reference = p != -2;
		}
		,getSource: function() {
			return this.ta;
		}
		,getTarget: function() {
			return this.tb;
		}
		,getSourceHeader: function() {
			return this.ia;
		}
		,getTargetHeader: function() {
			return this.ib;
		}
		,toOrder3: function() {
			var order = [];
			if(this.reference == null) {
				var $it0 = this.map_a2b.keys();
				while( $it0.hasNext() ) {
					var k = $it0.next();
					var unit = new coopy.Unit();
					unit.l = k;
					unit.r = this.a2b(k);
					order.push(unit);
				}
				var $it1 = this.map_b2a.keys();
				while( $it1.hasNext() ) {
					var k1 = $it1.next();
					if(this.b2a(k1) == -1) {
						var unit1 = new coopy.Unit();
						unit1.l = -1;
						unit1.r = k1;
						order.push(unit1);
					}
				}
			} else {
				var $it2 = this.map_a2b.keys();
				while( $it2.hasNext() ) {
					var k2 = $it2.next();
					var unit2 = new coopy.Unit();
					unit2.p = k2;
					unit2.l = this.reference.a2b(k2);
					unit2.r = this.a2b(k2);
					order.push(unit2);
				}
				var $it3 = this.reference.map_b2a.keys();
				while( $it3.hasNext() ) {
					var k3 = $it3.next();
					if(this.reference.b2a(k3) == -1) {
						var unit3 = new coopy.Unit();
						unit3.p = -1;
						unit3.l = k3;
						unit3.r = -1;
						order.push(unit3);
					}
				}
				var $it4 = this.map_b2a.keys();
				while( $it4.hasNext() ) {
					var k4 = $it4.next();
					if(this.b2a(k4) == -1) {
						var unit4 = new coopy.Unit();
						unit4.p = -1;
						unit4.l = -1;
						unit4.r = k4;
						order.push(unit4);
					}
				}
			}
			var top = order.length;
			var remotes = [];
			var locals = [];
			var _g = 0;
			while(_g < top) {
				var o = _g++;
				if(order[o].r >= 0) remotes.push(o); else locals.push(o);
			}
			var remote_sort = function(a,b) {
				return order[a].r - order[b].r;
			};
			var local_sort = function(a1,b1) {
				if(a1 == b1) return 0;
				if(order[a1].l >= 0 && order[b1].l >= 0) return order[a1].l - order[b1].l;
				if(order[a1].l >= 0) return 1;
				if(order[b1].l >= 0) return -1;
				return a1 - b1;
			};
			if(this.reference != null) {
				remote_sort = function(a2,b2) {
					if(a2 == b2) return 0;
					var o1 = order[a2].r - order[b2].r;
					if(order[a2].p >= 0 && order[b2].p >= 0) {
						var o2 = order[a2].p - order[b2].p;
						if(o1 * o2 < 0) return o1;
						var o3 = order[a2].l - order[b2].l;
						return o3;
					}
					return o1;
				};
				local_sort = function(a3,b3) {
					if(a3 == b3) return 0;
					if(order[a3].l >= 0 && order[b3].l >= 0) {
						var o11 = order[a3].l - order[b3].l;
						if(order[a3].p >= 0 && order[b3].p >= 0) {
							var o21 = order[a3].p - order[b3].p;
							if(o11 * o21 < 0) return o11;
							return o21;
						}
					}
					if(order[a3].l >= 0) return 1;
					if(order[b3].l >= 0) return -1;
					return a3 - b3;
				};
			}
			remotes.sort(remote_sort);
			locals.sort(local_sort);
			var revised_order = [];
			var at_r = 0;
			var at_l = 0;
			var _g1 = 0;
			while(_g1 < top) {
				var o4 = _g1++;
				if(at_r < remotes.length && at_l < locals.length) {
					var ur = order[remotes[at_r]];
					var ul = order[locals[at_l]];
					if(ul.l == -1 && ul.p >= 0 && ur.p >= 0) {
						if(ur.p > ul.p) {
							revised_order.push(ul);
							at_l++;
							continue;
						}
					} else if(ur.l > ul.l) {
						revised_order.push(ul);
						at_l++;
						continue;
					}
					revised_order.push(ur);
					at_r++;
					continue;
				}
				if(at_r < remotes.length) {
					var ur1 = order[remotes[at_r]];
					revised_order.push(ur1);
					at_r++;
					continue;
				}
				if(at_l < locals.length) {
					var ul1 = order[locals[at_l]];
					revised_order.push(ul1);
					at_l++;
					continue;
				}
			}
			order = revised_order;
			var result = new coopy.Ordering();
			result.setList(order);
			if(this.reference == null) result.ignoreParent();
			return result;
		}
		,markIdentical: function() {
			this.marked_as_identical = true;
		}
		,isMarkedAsIdentical: function() {
			return this.marked_as_identical;
		}
		,__class__: coopy.Alignment
	};
	coopy.CellBuilder = function() { };
	coopy.CellBuilder.__name__ = true;
	coopy.CellBuilder.prototype = {
		__class__: coopy.CellBuilder
	};
	coopy.CellInfo = $hx_exports.coopy.CellInfo = function() {
	};
	coopy.CellInfo.__name__ = true;
	coopy.CellInfo.prototype = {
		toString: function() {
			if(!this.updated) return this.value;
			if(!this.conflicted) return this.lvalue + "::" + this.rvalue;
			return this.pvalue + "||" + this.lvalue + "::" + this.rvalue;
		}
		,__class__: coopy.CellInfo
	};
	coopy.ColumnChange = $hx_exports.coopy.ColumnChange = function() {
	};
	coopy.ColumnChange.__name__ = true;
	coopy.ColumnChange.prototype = {
		__class__: coopy.ColumnChange
	};
	coopy.Table = function() { };
	coopy.Table.__name__ = true;
	coopy.Table.prototype = {
		__class__: coopy.Table
	};
	coopy.CombinedTable = $hx_exports.coopy.CombinedTable = function(t) {
		this.t = t;
		this.dx = 0;
		this.dy = 0;
		this.core = t;
		this.head = null;
		if(t.get_width() < 1 || t.get_height() < 1) return;
		var v = t.getCellView();
		if(v.toString(t.getCell(0,0)) != "@@") return;
		this.dx = 1;
		this.dy = 0;
		var _g1 = 0;
		var _g = t.get_height();
		while(_g1 < _g) {
			var y = _g1++;
			var txt = v.toString(t.getCell(0,y));
			if(txt == null || txt == "" || txt == "null") break;
			this.dy++;
		}
		this.head = new coopy.CombinedTableHead(this,this.dx,this.dy);
		this.body = new coopy.CombinedTableBody(this,this.dx,this.dy);
		this.core = this.body;
		this.meta = new coopy.SimpleMeta(this.head);
	};
	coopy.CombinedTable.__name__ = true;
	coopy.CombinedTable.__interfaces__ = [coopy.Table];
	coopy.CombinedTable.prototype = {
		all: function() {
			return this.t;
		}
		,getTable: function() {
			return this;
		}
		,get_width: function() {
			return this.core.get_width();
		}
		,get_height: function() {
			return this.core.get_height();
		}
		,getCell: function(x,y) {
			return this.core.getCell(x,y);
		}
		,setCell: function(x,y,c) {
			this.core.setCell(x,y,c);
		}
		,toString: function() {
			return coopy.SimpleTable.tableToString(this);
		}
		,getCellView: function() {
			return this.t.getCellView();
		}
		,isResizable: function() {
			return this.core.isResizable();
		}
		,resize: function(w,h) {
			return this.core.resize(h,w);
		}
		,clear: function() {
			this.core.clear();
		}
		,insertOrDeleteRows: function(fate,hfate) {
			return this.core.insertOrDeleteRows(fate,hfate);
		}
		,insertOrDeleteColumns: function(fate,wfate) {
			return this.core.insertOrDeleteColumns(fate,wfate);
		}
		,trimBlank: function() {
			return this.core.trimBlank();
		}
		,getData: function() {
			return null;
		}
		,clone: function() {
			return this.core.clone();
		}
		,create: function() {
			return this.t.create();
		}
		,getMeta: function() {
			return this.meta;
		}
		,__class__: coopy.CombinedTable
	};
	coopy.CombinedTableBody = function(parent,dx,dy) {
		this.parent = parent;
		this.dx = dx;
		this.dy = dy;
		this.all = parent.all();
	};
	coopy.CombinedTableBody.__name__ = true;
	coopy.CombinedTableBody.__interfaces__ = [coopy.Table];
	coopy.CombinedTableBody.prototype = {
		getTable: function() {
			return this;
		}
		,get_width: function() {
			return this.all.get_width() - 1;
		}
		,get_height: function() {
			return this.all.get_height() - this.dy + 1;
		}
		,getCell: function(x,y) {
			if(y == 0) {
				if(this.meta == null) this.meta = this.parent.getMeta().asTable();
				return this.meta.getCell(x + this.dx,0);
			}
			return this.all.getCell(x + this.dx,y + this.dy - 1);
		}
		,setCell: function(x,y,c) {
			if(y == 0) {
				this.all.setCell(x + this.dx,0,c);
				return;
			}
			this.all.setCell(x + this.dx,y + this.dy - 1,c);
		}
		,toString: function() {
			return coopy.SimpleTable.tableToString(this);
		}
		,getCellView: function() {
			return this.all.getCellView();
		}
		,isResizable: function() {
			return this.all.isResizable();
		}
		,resize: function(w,h) {
			return this.all.resize(w + 1,h + this.dy);
		}
		,clear: function() {
			this.all.clear();
			this.dx = 0;
			this.dy = 0;
		}
		,insertOrDeleteRows: function(fate,hfate) {
			var fate2 = [];
			var _g1 = 0;
			var _g = this.dy;
			while(_g1 < _g) {
				var y = _g1++;
				fate2.push(y);
			}
			var hdr = true;
			var _g2 = 0;
			while(_g2 < fate.length) {
				var f = fate[_g2];
				++_g2;
				if(hdr) {
					hdr = false;
					continue;
				}
				fate2.push(f >= 0?f + this.dy - 1:f);
			}
			return this.all.insertOrDeleteRows(fate2,hfate + this.dy - 1);
		}
		,insertOrDeleteColumns: function(fate,wfate) {
			var fate2 = [];
			var _g1 = 0;
			var _g = this.dx + 1;
			while(_g1 < _g) {
				var x = _g1++;
				fate2.push(x);
			}
			var _g2 = 0;
			while(_g2 < fate.length) {
				var f = fate[_g2];
				++_g2;
				fate2.push(f >= 0?f + this.dx + 1:f);
			}
			return this.all.insertOrDeleteColumns(fate2,wfate + this.dx);
		}
		,trimBlank: function() {
			return this.all.trimBlank();
		}
		,getData: function() {
			return null;
		}
		,clone: function() {
			return new coopy.CombinedTable(this.all.clone());
		}
		,create: function() {
			return new coopy.CombinedTable(this.all.create());
		}
		,getMeta: function() {
			return this.parent.getMeta();
		}
		,__class__: coopy.CombinedTableBody
	};
	coopy.CombinedTableHead = function(parent,dx,dy) {
		this.parent = parent;
		this.dx = dx;
		this.dy = dy;
		this.all = parent.all();
	};
	coopy.CombinedTableHead.__name__ = true;
	coopy.CombinedTableHead.__interfaces__ = [coopy.Table];
	coopy.CombinedTableHead.prototype = {
		getTable: function() {
			return this;
		}
		,get_width: function() {
			return this.all.get_width();
		}
		,get_height: function() {
			return this.dy;
		}
		,getCell: function(x,y) {
			if(x == 0) {
				var v = this.getCellView();
				var txt = v.toString(this.all.getCell(x,y));
				if(txt.charAt(0) == "@") return HxOverrides.substr(txt,1,txt.length);
			}
			return this.all.getCell(x,y);
		}
		,setCell: function(x,y,c) {
			this.all.setCell(x,y,c);
		}
		,toString: function() {
			return coopy.SimpleTable.tableToString(this);
		}
		,getCellView: function() {
			return this.all.getCellView();
		}
		,isResizable: function() {
			return false;
		}
		,resize: function(w,h) {
			return false;
		}
		,clear: function() {
		}
		,insertOrDeleteRows: function(fate,hfate) {
			return false;
		}
		,insertOrDeleteColumns: function(fate,wfate) {
			return this.all.insertOrDeleteColumns(fate,wfate);
		}
		,trimBlank: function() {
			return false;
		}
		,getData: function() {
			return null;
		}
		,clone: function() {
			return null;
		}
		,create: function() {
			return null;
		}
		,getMeta: function() {
			return null;
		}
		,__class__: coopy.CombinedTableHead
	};
	coopy.CompareFlags = $hx_exports.coopy.CompareFlags = function() {
		this.ordered = true;
		this.show_unchanged = false;
		this.unchanged_context = 1;
		this.always_show_order = false;
		this.never_show_order = true;
		this.show_unchanged_columns = false;
		this.unchanged_column_context = 1;
		this.always_show_header = true;
		this.acts = null;
		this.ids = null;
		this.columns_to_ignore = null;
		this.allow_nested_cells = false;
		this.warnings = null;
		this.diff_strategy = null;
		this.show_meta = true;
		this.show_unchanged_meta = false;
		this.tables = null;
		this.parent = null;
		this.count_like_a_spreadsheet = true;
		this.ignore_whitespace = false;
		this.ignore_case = false;
		this.terminal_format = null;
	};
	coopy.CompareFlags.__name__ = true;
	coopy.CompareFlags.prototype = {
		filter: function(act,allow) {
			if(this.acts == null) {
				this.acts = new haxe.ds.StringMap();
				this.acts.set("update",!allow);
				this.acts.set("insert",!allow);
				this.acts.set("delete",!allow);
			}
			if(!this.acts.exists(act)) return false;
			this.acts.set(act,allow);
			return true;
		}
		,allowUpdate: function() {
			if(this.acts == null) return true;
			return this.acts.exists("update");
		}
		,allowInsert: function() {
			if(this.acts == null) return true;
			return this.acts.exists("insert");
		}
		,allowDelete: function() {
			if(this.acts == null) return true;
			return this.acts.exists("delete");
		}
		,getIgnoredColumns: function() {
			if(this.columns_to_ignore == null) return null;
			var ignore = new haxe.ds.StringMap();
			var _g1 = 0;
			var _g = this.columns_to_ignore.length;
			while(_g1 < _g) {
				var i = _g1++;
				ignore.set(this.columns_to_ignore[i],true);
			}
			return ignore;
		}
		,addPrimaryKey: function(column) {
			if(this.ids == null) this.ids = [];
			this.ids.push(column);
		}
		,ignoreColumn: function(column) {
			if(this.columns_to_ignore == null) this.columns_to_ignore = [];
			this.columns_to_ignore.push(column);
		}
		,addTable: function(table) {
			if(this.tables == null) this.tables = [];
			this.tables.push(table);
		}
		,addWarning: function(warn) {
			if(this.warnings == null) this.warnings = [];
			this.warnings.push(warn);
		}
		,getWarning: function() {
			return this.warnings.join("\n");
		}
		,__class__: coopy.CompareFlags
	};
	coopy.CompareTable = $hx_exports.coopy.CompareTable = function(comp) {
		this.comp = comp;
		if(comp.compare_flags != null) {
			if(comp.compare_flags.parent != null) comp.p = comp.compare_flags.parent;
		}
	};
	coopy.CompareTable.__name__ = true;
	coopy.CompareTable.prototype = {
		run: function() {
			if(this.useSql()) {
				this.comp.completed = true;
				return false;
			}
			var more = this.compareCore();
			while(more && this.comp.run_to_completion) more = this.compareCore();
			return !more;
		}
		,align: function() {
			while(!this.comp.completed) this.run();
			var alignment = new coopy.Alignment();
			this.alignCore(alignment);
			alignment.comp = this.comp;
			this.comp.alignment = alignment;
			return alignment;
		}
		,getComparisonState: function() {
			return this.comp;
		}
		,alignCore: function(align) {
			if(this.useSql()) {
				var tab1 = null;
				var tab2 = null;
				var tab3 = null;
				if(this.comp.p == null) {
					tab1 = this.comp.a;
					tab2 = this.comp.b;
				} else {
					align.reference = new coopy.Alignment();
					tab1 = this.comp.p;
					tab2 = this.comp.b;
					tab3 = this.comp.a;
				}
				var db = null;
				if(tab1 != null) db = tab1.getDatabase();
				if(db == null && tab2 != null) db = tab2.getDatabase();
				if(db == null && tab3 != null) db = tab3.getDatabase();
				var sc = new coopy.SqlCompare(db,tab1,tab2,tab3,align);
				sc.apply();
				if(this.comp.p != null) align.meta.reference = align.reference.meta;
				return;
			}
			if(this.comp.p == null) {
				this.alignCore2(align,this.comp.a,this.comp.b);
				return;
			}
			align.reference = new coopy.Alignment();
			this.alignCore2(align,this.comp.p,this.comp.b);
			this.alignCore2(align.reference,this.comp.p,this.comp.a);
			align.meta.reference = align.reference.meta;
		}
		,alignCore2: function(align,a,b) {
			if(align.meta == null) align.meta = new coopy.Alignment();
			this.alignColumns(align.meta,a,b);
			var column_order = align.meta.toOrder();
			align.range(a.get_height(),b.get_height());
			align.tables(a,b);
			align.setRowlike(true);
			var w = a.get_width();
			var ha = a.get_height();
			var hb = b.get_height();
			var av = a.getCellView();
			var ids = null;
			var ignore = null;
			if(this.comp.compare_flags != null) {
				ids = this.comp.compare_flags.ids;
				ignore = this.comp.compare_flags.getIgnoredColumns();
			}
			var common_units = [];
			var ra_header = align.getSourceHeader();
			var rb_header = align.getSourceHeader();
			var _g = 0;
			var _g1 = column_order.getList();
			while(_g < _g1.length) {
				var unit = _g1[_g];
				++_g;
				if(unit.l >= 0 && unit.r >= 0 && unit.p != -1) {
					if(ignore != null) {
						if(unit.l >= 0 && ra_header >= 0 && ra_header < a.get_height()) {
							var name = av.toString(a.getCell(unit.l,ra_header));
							if(__map_reserved[name] != null?ignore.existsReserved(name):ignore.h.hasOwnProperty(name)) continue;
						}
						if(unit.r >= 0 && rb_header >= 0 && rb_header < b.get_height()) {
							var name1 = av.toString(b.getCell(unit.r,rb_header));
							if(__map_reserved[name1] != null?ignore.existsReserved(name1):ignore.h.hasOwnProperty(name1)) continue;
						}
					}
					common_units.push(unit);
				}
			}
			var index_top = null;
			var pending_ct = ha;
			var reverse_pending_ct = hb;
			var used_h = { };
			var used_reverse_h = { };
			if(ids != null) {
				index_top = new coopy.IndexPair(this.comp.compare_flags);
				var ids_as_map = new haxe.ds.StringMap();
				var _g2 = 0;
				while(_g2 < ids.length) {
					var id = ids[_g2];
					++_g2;
					{
						if(__map_reserved[id] != null) ids_as_map.setReserved(id,true); else ids_as_map.h[id] = true;
						true;
					}
				}
				var _g3 = 0;
				while(_g3 < common_units.length) {
					var unit1 = common_units[_g3];
					++_g3;
					var na = av.toString(a.getCell(unit1.l,0));
					var nb = av.toString(b.getCell(unit1.r,0));
					if((__map_reserved[na] != null?ids_as_map.existsReserved(na):ids_as_map.h.hasOwnProperty(na)) || (__map_reserved[nb] != null?ids_as_map.existsReserved(nb):ids_as_map.h.hasOwnProperty(nb))) {
						index_top.addColumns(unit1.l,unit1.r);
						align.addIndexColumns(unit1);
					}
				}
				index_top.indexTables(a,b,1);
				if(this.indexes != null) this.indexes.push(index_top);
				var _g4 = 0;
				while(_g4 < ha) {
					var j = _g4++;
					var cross = index_top.queryLocal(j);
					var spot_a = cross.spot_a;
					var spot_b = cross.spot_b;
					if(spot_a != 1 || spot_b != 1) continue;
					var jb = cross.item_b.lst[0];
					align.link(j,jb);
					used_h[jb] = 1;
					if(!used_reverse_h.hasOwnProperty(j)) reverse_pending_ct--;
					used_reverse_h[j] = 1;
				}
			} else {
				var N = 5;
				var columns = [];
				if(common_units.length > N) {
					var columns_eval = [];
					var _g11 = 0;
					var _g5 = common_units.length;
					while(_g11 < _g5) {
						var i = _g11++;
						var ct = 0;
						var mem = new haxe.ds.StringMap();
						var mem2 = new haxe.ds.StringMap();
						var ca = common_units[i].l;
						var cb = common_units[i].r;
						var _g21 = 0;
						while(_g21 < ha) {
							var j1 = _g21++;
							var key = av.toString(a.getCell(ca,j1));
							if(!(__map_reserved[key] != null?mem.existsReserved(key):mem.h.hasOwnProperty(key))) {
								if(__map_reserved[key] != null) mem.setReserved(key,1); else mem.h[key] = 1;
								ct++;
							}
						}
						var _g22 = 0;
						while(_g22 < hb) {
							var j2 = _g22++;
							var key1 = av.toString(b.getCell(cb,j2));
							if(!(__map_reserved[key1] != null?mem2.existsReserved(key1):mem2.h.hasOwnProperty(key1))) {
								if(__map_reserved[key1] != null) mem2.setReserved(key1,1); else mem2.h[key1] = 1;
								ct++;
							}
						}
						columns_eval.push([i,ct]);
					}
					var sorter = function(a1,b1) {
						if(a1[1] < b1[1]) return 1;
						if(a1[1] > b1[1]) return -1;
						if(a1[0] > b1[0]) return 1;
						if(a1[0] < b1[0]) return -1;
						return 0;
					};
					columns_eval.sort(sorter);
					columns = Lambda.array(Lambda.map(columns_eval,function(v) {
						return v[0];
					}));
					columns = columns.slice(0,N);
				} else {
					var _g12 = 0;
					var _g6 = common_units.length;
					while(_g12 < _g6) {
						var i1 = _g12++;
						columns.push(i1);
					}
				}
				var top = Math.round(Math.pow(2,columns.length));
				var pending = new haxe.ds.IntMap();
				var _g7 = 0;
				while(_g7 < ha) {
					var j3 = _g7++;
					pending.h[j3] = j3;
				}
				var added_columns_h = { };
				var index_ct = 0;
				var _g8 = 0;
				while(_g8 < top) {
					var k = _g8++;
					if(k == 0) continue;
					if(pending_ct == 0) break;
					var active_columns = [];
					var kk = k;
					var at = 0;
					while(kk > 0) {
						if(kk % 2 == 1) active_columns.push(columns[at]);
						kk >>= 1;
						at++;
					}
					var index = new coopy.IndexPair(this.comp.compare_flags);
					var _g23 = 0;
					var _g13 = active_columns.length;
					while(_g23 < _g13) {
						var k1 = _g23++;
						var col = active_columns[k1];
						var unit2 = common_units[col];
						index.addColumns(unit2.l,unit2.r);
						if(!added_columns_h.hasOwnProperty(col)) {
							align.addIndexColumns(unit2);
							added_columns_h[col] = true;
						}
					}
					index.indexTables(a,b,1);
					if(k == top - 1) index_top = index;
					var h = a.get_height();
					if(b.get_height() > h) h = b.get_height();
					if(h < 1) h = 1;
					var wide_top_freq = index.getTopFreq();
					var ratio = wide_top_freq;
					ratio /= h + 20;
					if(ratio >= 0.1) {
						if(index_ct > 0 || k < top - 1) continue;
					}
					index_ct++;
					if(this.indexes != null) this.indexes.push(index);
					var fixed = [];
					var $it0 = pending.keys();
					while( $it0.hasNext() ) {
						var j4 = $it0.next();
						var cross1 = index.queryLocal(j4);
						var spot_a1 = cross1.spot_a;
						var spot_b1 = cross1.spot_b;
						if(spot_a1 != 1 || spot_b1 != 1) continue;
						var val = cross1.item_b.lst[0];
						if(!used_h.hasOwnProperty(val)) {
							fixed.push(j4);
							align.link(j4,val);
							used_h[val] = 1;
							if(!used_reverse_h.hasOwnProperty(j4)) reverse_pending_ct--;
							used_reverse_h[j4] = 1;
						}
					}
					var _g24 = 0;
					var _g14 = fixed.length;
					while(_g24 < _g14) {
						var j5 = _g24++;
						pending.remove(fixed[j5]);
						pending_ct--;
					}
				}
			}
			if(index_top != null) {
				var offset = 0;
				var scale = 1;
				var _g9 = 0;
				while(_g9 < 2) {
					var sgn = _g9++;
					if(pending_ct > 0) {
						var xb = null;
						if(scale == -1 && hb > 0) xb = hb - 1;
						var _g15 = 0;
						while(_g15 < ha) {
							var xa0 = _g15++;
							var xa = xa0 * scale + offset;
							var xb2 = align.a2b(xa);
							if(xb2 != null) {
								xb = xb2 + scale;
								if(xb >= hb || xb < 0) break;
								continue;
							}
							if(xb == null) continue;
							var ka = index_top.localKey(xa);
							var kb = index_top.remoteKey(xb);
							if(ka != kb) continue;
							if(used_h.hasOwnProperty(xb)) continue;
							align.link(xa,xb);
							used_h[xb] = 1;
							used_reverse_h[xa] = 1;
							pending_ct--;
							xb += scale;
							if(xb >= hb || xb < 0) break;
							if(pending_ct == 0) break;
						}
					}
					offset = ha - 1;
					scale = -1;
				}
				offset = 0;
				scale = 1;
				var _g10 = 0;
				while(_g10 < 2) {
					var sgn1 = _g10++;
					if(reverse_pending_ct > 0) {
						var xa1 = null;
						if(scale == -1 && ha > 0) xa1 = ha - 1;
						var _g16 = 0;
						while(_g16 < hb) {
							var xb0 = _g16++;
							var xb1 = xb0 * scale + offset;
							var xa2 = align.b2a(xb1);
							if(xa2 != null) {
								xa1 = xa2 + scale;
								if(xa1 >= ha || xa1 < 0) break;
								continue;
							}
							if(xa1 == null) continue;
							var ka1 = index_top.localKey(xa1);
							var kb1 = index_top.remoteKey(xb1);
							if(ka1 != kb1) continue;
							if(used_reverse_h.hasOwnProperty(xa1)) continue;
							align.link(xa1,xb1);
							used_h[xb1] = 1;
							used_reverse_h[xa1] = 1;
							reverse_pending_ct--;
							xa1 += scale;
							if(xa1 >= ha || xa1 < 0) break;
							if(reverse_pending_ct == 0) break;
						}
					}
					offset = hb - 1;
					scale = -1;
				}
			}
			var _g17 = 1;
			while(_g17 < ha) {
				var i2 = _g17++;
				if(!used_reverse_h.hasOwnProperty(i2)) align.link(i2,-1);
			}
			var _g18 = 1;
			while(_g18 < hb) {
				var i3 = _g18++;
				if(!used_h.hasOwnProperty(i3)) align.link(-1,i3);
			}
			if(ha > 0 && hb > 0) align.link(0,0);
		}
		,alignColumns: function(align,a,b) {
			align.range(a.get_width(),b.get_width());
			align.tables(a,b);
			align.setRowlike(false);
			var slop = 5;
			var va = a.getCellView();
			var vb = b.getCellView();
			var ra_best = 0;
			var rb_best = 0;
			var ct_best = -1;
			var ma_best = null;
			var mb_best = null;
			var ra_header = 0;
			var rb_header = 0;
			var ra_uniques = 0;
			var rb_uniques = 0;
			var _g = 0;
			while(_g < slop) {
				var ra = _g++;
				var _g1 = 0;
				while(_g1 < slop) {
					var rb = _g1++;
					var ma = new haxe.ds.StringMap();
					var mb = new haxe.ds.StringMap();
					var ct = 0;
					var uniques = 0;
					if(ra < a.get_height()) {
						var _g3 = 0;
						var _g2 = a.get_width();
						while(_g3 < _g2) {
							var ca = _g3++;
							var key = va.toString(a.getCell(ca,ra));
							if(__map_reserved[key] != null?ma.existsReserved(key):ma.h.hasOwnProperty(key)) {
								if(__map_reserved[key] != null) ma.setReserved(key,-1); else ma.h[key] = -1;
								uniques--;
							} else {
								if(__map_reserved[key] != null) ma.setReserved(key,ca); else ma.h[key] = ca;
								uniques++;
							}
						}
						if(uniques > ra_uniques) {
							ra_header = ra;
							ra_uniques = uniques;
						}
					}
					uniques = 0;
					if(rb < b.get_height()) {
						var _g31 = 0;
						var _g21 = b.get_width();
						while(_g31 < _g21) {
							var cb = _g31++;
							var key1 = vb.toString(b.getCell(cb,rb));
							if(__map_reserved[key1] != null?mb.existsReserved(key1):mb.h.hasOwnProperty(key1)) {
								if(__map_reserved[key1] != null) mb.setReserved(key1,-1); else mb.h[key1] = -1;
								uniques--;
							} else {
								if(__map_reserved[key1] != null) mb.setReserved(key1,cb); else mb.h[key1] = cb;
								uniques++;
							}
						}
						if(uniques > rb_uniques) {
							rb_header = rb;
							rb_uniques = uniques;
						}
					}
					var $it0 = ma.keys();
					while( $it0.hasNext() ) {
						var key2 = $it0.next();
						var i0;
						i0 = __map_reserved[key2] != null?ma.getReserved(key2):ma.h[key2];
						var i1;
						i1 = __map_reserved[key2] != null?mb.getReserved(key2):mb.h[key2];
						if(i1 != null) {
							if(i1 >= 0 && i0 >= 0) ct++;
						}
					}
					if(ct > ct_best) {
						ct_best = ct;
						ma_best = ma;
						mb_best = mb;
						ra_best = ra;
						rb_best = rb;
					}
				}
			}
			if(ma_best == null) {
				if(a.get_height() > 0 && b.get_height() == 0) align.headers(0,-1); else if(a.get_height() == 0 && b.get_height() > 0) align.headers(-1,0);
				return;
			}
			var $it1 = ma_best.keys();
			while( $it1.hasNext() ) {
				var key3 = $it1.next();
				var i01;
				i01 = __map_reserved[key3] != null?ma_best.getReserved(key3):ma_best.h[key3];
				var i11;
				i11 = __map_reserved[key3] != null?mb_best.getReserved(key3):mb_best.h[key3];
				if(i01 != null && i11 != null) align.link(i01,i11); else if(i01 != null) align.link(i01,-1); else if(i11 != null) align.link(-1,i11);
			}
			var $it2 = mb_best.keys();
			while( $it2.hasNext() ) {
				var key4 = $it2.next();
				var i02;
				i02 = __map_reserved[key4] != null?ma_best.getReserved(key4):ma_best.h[key4];
				var i12;
				i12 = __map_reserved[key4] != null?mb_best.getReserved(key4):mb_best.h[key4];
				if(i02 == null && i12 != null) align.link(-1,i12);
			}
			align.headers(ra_header,rb_header);
		}
		,testHasSameColumns: function() {
			var p = this.comp.p;
			var a = this.comp.a;
			var b = this.comp.b;
			var eq = this.hasSameColumns2(a,b);
			if(eq && p != null) eq = this.hasSameColumns2(p,a);
			this.comp.has_same_columns = eq;
			this.comp.has_same_columns_known = true;
			return true;
		}
		,hasSameColumns2: function(a,b) {
			if(a.get_width() != b.get_width()) return false;
			if(a.get_height() == 0 || b.get_height() == 0) return true;
			var av = a.getCellView();
			var _g1 = 0;
			var _g = a.get_width();
			while(_g1 < _g) {
				var i = _g1++;
				var _g3 = i + 1;
				var _g2 = a.get_width();
				while(_g3 < _g2) {
					var j = _g3++;
					if(av.equals(a.getCell(i,0),a.getCell(j,0))) return false;
				}
				if(!av.equals(a.getCell(i,0),b.getCell(i,0))) return false;
			}
			return true;
		}
		,testIsEqual: function() {
			var p = this.comp.p;
			var a = this.comp.a;
			var b = this.comp.b;
			this.comp.getMeta();
			var nested = false;
			if(this.comp.p_meta != null) {
				if(this.comp.p_meta.isNested()) nested = true;
			}
			if(this.comp.a_meta != null) {
				if(this.comp.a_meta.isNested()) nested = true;
			}
			if(this.comp.b_meta != null) {
				if(this.comp.b_meta.isNested()) nested = true;
			}
			if(nested) {
				this.comp.is_equal = false;
				this.comp.is_equal_known = true;
				return true;
			}
			var eq = this.isEqual2(a,b);
			if(eq && p != null) eq = this.isEqual2(p,a);
			this.comp.is_equal = eq;
			this.comp.is_equal_known = true;
			return true;
		}
		,isEqual2: function(a,b) {
			if(a.get_width() != b.get_width() || a.get_height() != b.get_height()) return false;
			var av = a.getCellView();
			var _g1 = 0;
			var _g = a.get_height();
			while(_g1 < _g) {
				var i = _g1++;
				var _g3 = 0;
				var _g2 = a.get_width();
				while(_g3 < _g2) {
					var j = _g3++;
					if(!av.equals(a.getCell(j,i),b.getCell(j,i))) return false;
				}
			}
			return true;
		}
		,compareCore: function() {
			if(this.comp.completed) return false;
			if(!this.comp.is_equal_known) return this.testIsEqual();
			if(!this.comp.has_same_columns_known) return this.testHasSameColumns();
			this.comp.completed = true;
			return false;
		}
		,storeIndexes: function() {
			this.indexes = [];
		}
		,getIndexes: function() {
			return this.indexes;
		}
		,useSql: function() {
			if(this.comp.compare_flags == null) return false;
			this.comp.getMeta();
			var sql = true;
			if(this.comp.p_meta != null) {
				if(!this.comp.p_meta.isSql()) sql = false;
			}
			if(this.comp.a_meta != null) {
				if(!this.comp.a_meta.isSql()) sql = false;
			}
			if(this.comp.b_meta != null) {
				if(!this.comp.b_meta.isSql()) sql = false;
			}
			if(this.comp.p != null && this.comp.p_meta == null) sql = false;
			if(this.comp.a != null && this.comp.a_meta == null) sql = false;
			if(this.comp.b != null && this.comp.b_meta == null) sql = false;
			return sql;
		}
		,__class__: coopy.CompareTable
	};
	coopy.Coopy = $hx_exports.coopy.Coopy = function(io) {
		this.init();
		this.io = io;
	};
	coopy.Coopy.__name__ = true;
	coopy.Coopy.diffAsHtml = function(local,remote,flags) {
		var comp = new coopy.TableComparisonState();
		var td = coopy.Coopy.align(local,remote,flags,comp);
		var o = coopy.Coopy.getBlankTable(td,comp);
		if(comp.a != null) o = comp.a.create();
		if(o == null && comp.b != null) o = comp.b.create();
		if(o == null) o = new coopy.SimpleTable(0,0);
		var os = new coopy.Tables(o);
		td.hiliteWithNesting(os);
		var render = new coopy.DiffRender();
		return render.renderTables(os).html();
	};
	coopy.Coopy.diffAsAnsi = function(local,remote,flags) {
		var tool = new coopy.Coopy(new coopy.TableIO());
		tool.cache_txt = "";
		if(flags == null) flags = new coopy.CompareFlags();
		tool.output_format = "csv";
		tool.runDiff(flags.parent,local,remote,flags,null);
		return tool.cache_txt;
	};
	coopy.Coopy.diff = function(local,remote,flags) {
		var comp = new coopy.TableComparisonState();
		var td = coopy.Coopy.align(local,remote,flags,comp);
		var o = coopy.Coopy.getBlankTable(td,comp);
		if(comp.a != null) o = comp.a.create();
		if(o == null && comp.b != null) o = comp.b.create();
		if(o == null) o = new coopy.SimpleTable(0,0);
		td.hilite(o);
		return o;
	};
	coopy.Coopy.getBlankTable = function(td,comp) {
		var o = null;
		if(comp.a != null) o = comp.a.create();
		if(o == null && comp.b != null) o = comp.b.create();
		if(o == null) o = new coopy.SimpleTable(0,0);
		return o;
	};
	coopy.Coopy.align = function(local,remote,flags,comp) {
		comp.a = coopy.Coopy.tablify(local);
		comp.b = coopy.Coopy.tablify(remote);
		if(flags == null) flags = new coopy.CompareFlags();
		comp.compare_flags = flags;
		var ct = new coopy.CompareTable(comp);
		var align = ct.align();
		var td = new coopy.TableDiff(align,flags);
		return td;
	};
	coopy.Coopy.patch = function(local,patch,flags) {
		var patcher = new coopy.HighlightPatch(coopy.Coopy.tablify(local),coopy.Coopy.tablify(patch));
		return patcher.apply();
	};
	coopy.Coopy.compareTables = function(local,remote,flags) {
		var comp = new coopy.TableComparisonState();
		comp.a = coopy.Coopy.tablify(local);
		comp.b = coopy.Coopy.tablify(remote);
		comp.compare_flags = flags;
		var ct = new coopy.CompareTable(comp);
		return ct;
	};
	coopy.Coopy.compareTables3 = function(parent,local,remote,flags) {
		var comp = new coopy.TableComparisonState();
		comp.p = coopy.Coopy.tablify(parent);
		comp.a = coopy.Coopy.tablify(local);
		comp.b = coopy.Coopy.tablify(remote);
		comp.compare_flags = flags;
		var ct = new coopy.CompareTable(comp);
		return ct;
	};
	coopy.Coopy.keepAround = function() {
		var st = new coopy.SimpleTable(1,1);
		var v = new coopy.Viterbi();
		var td = new coopy.TableDiff(null,null);
		var cf = new coopy.CompareFlags();
		var idx = new coopy.Index(cf);
		var dr = new coopy.DiffRender();
		var hp = new coopy.HighlightPatch(null,null);
		var csv = new coopy.Csv();
		var tm = new coopy.TableModifier(null);
		var sc = new coopy.SqlCompare(null,null,null,null);
		var sq = new coopy.SqliteHelper();
		var sm = new coopy.SimpleMeta(null);
		var ct = new coopy.CombinedTable(null);
		return 0;
	};
	coopy.Coopy.cellFor = function(x) {
		return x;
	};
	coopy.Coopy.main = function() {
		return 0;
	};
	coopy.Coopy.show = function(t) {
		var w = t.get_width();
		var h = t.get_height();
		var txt = "";
		var _g = 0;
		while(_g < h) {
			var y = _g++;
			var _g1 = 0;
			while(_g1 < w) {
				var x = _g1++;
				txt += Std.string(t.getCell(x,y));
				txt += " ";
			}
			txt += "\n";
		}
		console.log(txt);
	};
	coopy.Coopy.jsonify = function(t) {
		var workbook = new haxe.ds.StringMap();
		var sheet = [];
		var w = t.get_width();
		var h = t.get_height();
		var txt = "";
		var _g = 0;
		while(_g < h) {
			var y = _g++;
			var row = [];
			var _g1 = 0;
			while(_g1 < w) {
				var x = _g1++;
				var v = t.getCell(x,y);
				row.push(v);
			}
			sheet.push(row);
		}
		if(__map_reserved.sheet != null) workbook.setReserved("sheet",sheet); else workbook.h["sheet"] = sheet;
		return workbook;
	};
	coopy.Coopy.tablify = function(data) {
		if(data == null) return data;
		var get_cell_view = Reflect.field(data,"getCellView");
		if(get_cell_view != null) return data;
		return new (typeof window != 'undefined' ? window : exports).daff.TableView(data);
	};
	coopy.Coopy.prototype = {
		init: function() {
			this.extern_preference = false;
			this.format_preference = null;
			this.delim_preference = null;
			this.output_format = "copy";
			this.output_format_set = false;
			this.nested_output = false;
			this.order_set = false;
			this.order_preference = false;
			this.strategy = null;
			this.pretty = true;
			this.css_output = null;
			this.fragment = false;
			this.flags = null;
			this.cache_txt = null;
		}
		,checkFormat: function(name) {
			if(this.extern_preference) return this.format_preference;
			var ext = "";
			if(name != null) {
				var pt = name.lastIndexOf(".");
				if(pt >= 0) {
					ext = HxOverrides.substr(name,pt + 1,null).toLowerCase();
					switch(ext) {
					case "json":
						this.format_preference = "json";
						break;
					case "ndjson":
						this.format_preference = "ndjson";
						break;
					case "csv":
						this.format_preference = "csv";
						this.delim_preference = ",";
						break;
					case "tsv":
						this.format_preference = "csv";
						this.delim_preference = "\t";
						break;
					case "ssv":
						this.format_preference = "csv";
						this.delim_preference = ";";
						break;
					case "sqlite3":
						this.format_preference = "sqlite";
						break;
					case "sqlite":
						this.format_preference = "sqlite";
						break;
					case "html":case "htm":
						this.format_preference = "html";
						break;
					case "www":
						this.format_preference = "www";
						break;
					default:
						ext = "";
					}
				}
			}
			this.nested_output = this.format_preference == "json" || this.format_preference == "ndjson";
			this.order_preference = !this.nested_output;
			return ext;
		}
		,setFormat: function(name) {
			this.extern_preference = false;
			this.checkFormat("." + name);
			this.extern_preference = true;
		}
		,getRenderer: function() {
			var renderer = new coopy.DiffRender();
			renderer.usePrettyArrows(this.pretty);
			return renderer;
		}
		,applyRenderer: function(name,renderer) {
			if(!this.fragment) renderer.completeHtml();
			if(this.format_preference == "www") this.io.sendToBrowser(renderer.html()); else this.saveText(name,renderer.html());
			if(this.css_output != null) this.saveText(this.css_output,renderer.sampleCss());
			return true;
		}
		,renderTable: function(name,t) {
			var renderer = this.getRenderer();
			renderer.render(t);
			return this.applyRenderer(name,renderer);
		}
		,renderTables: function(name,t) {
			var renderer = this.getRenderer();
			renderer.renderTables(t);
			return this.applyRenderer(name,renderer);
		}
		,saveTable: function(name,t,render) {
			var txt = this.encodeTable(name,t,render);
			return this.saveText(name,txt);
		}
		,encodeTable: function(name,t,render) {
			if(this.output_format != "copy") this.setFormat(this.output_format);
			var txt = "";
			this.checkFormat(name);
			if(this.format_preference == "sqlite" && !this.extern_preference) this.format_preference = "csv";
			if(render == null) {
				if(this.format_preference == "csv") {
					var csv = new coopy.Csv(this.delim_preference);
					txt = csv.renderTable(t);
				} else if(this.format_preference == "ndjson") txt = new coopy.Ndjson(t).render(); else if(this.format_preference == "html" || this.format_preference == "www") this.renderTable(name,t); else if(this.format_preference == "sqlite") {
					this.io.writeStderr("! Cannot yet output to sqlite, aborting\n");
					return "";
				} else txt = haxe.Json.stringify(coopy.Coopy.jsonify(t),null,"  ");
			} else txt = render.render(t);
			return txt;
		}
		,saveTables: function(name,os,use_color) {
			if(this.output_format != "copy") this.setFormat(this.output_format);
			var txt = "";
			this.checkFormat(name);
			var render = null;
			if(use_color) render = new coopy.TerminalDiffRender(this.flags);
			var order = os.getOrder();
			if(order.length == 1) return this.saveTable(name,os.one(),render);
			if(this.format_preference == "html" || this.format_preference == "www") return this.renderTables(name,os);
			var need_blank = false;
			if(order.length == 0 || os.hasInsDel()) {
				txt += this.encodeTable(name,os.one(),render);
				need_blank = true;
			}
			if(order.length > 1) {
				var _g1 = 1;
				var _g = order.length;
				while(_g1 < _g) {
					var i = _g1++;
					var t = os.get(order[i]);
					if(t != null) {
						if(need_blank) txt += "\n";
						need_blank = true;
						txt += order[i] + "\n";
						var line = "";
						var _g3 = 0;
						var _g2 = order[i].length;
						while(_g3 < _g2) {
							var i1 = _g3++;
							line += "=";
						}
						txt += line + "\n";
						txt += this.encodeTable(name,os.get(order[i]),render);
					}
				}
			}
			return this.saveText(name,txt);
		}
		,saveText: function(name,txt) {
			if(name == null) this.cache_txt += txt; else if(name != "-") this.io.saveContent(name,txt); else this.io.writeStdout(txt);
			return true;
		}
		,jsonToTables: function(json) {
			var tables = Reflect.field(json,"tables");
			if(tables == null) return this.jsonToTable(json);
			return new coopy.JsonTables(json,this.flags);
		}
		,jsonToTable: function(json) {
			var output = null;
			var _g = 0;
			var _g1 = Reflect.fields(json);
			while(_g < _g1.length) {
				var name = _g1[_g];
				++_g;
				var t = Reflect.field(json,name);
				var columns = Reflect.field(t,"columns");
				if(columns == null) continue;
				var rows = Reflect.field(t,"rows");
				if(rows == null) continue;
				output = new coopy.SimpleTable(columns.length,rows.length);
				var has_hash = false;
				var has_hash_known = false;
				var _g3 = 0;
				var _g2 = rows.length;
				while(_g3 < _g2) {
					var i = _g3++;
					var row = rows[i];
					if(!has_hash_known) {
						if(Reflect.fields(row).length == columns.length) has_hash = true;
						has_hash_known = true;
					}
					if(!has_hash) {
						var lst = row;
						var _g5 = 0;
						var _g4 = columns.length;
						while(_g5 < _g4) {
							var j = _g5++;
							var val = lst[j];
							output.setCell(j,i,coopy.Coopy.cellFor(val));
						}
					} else {
						var _g51 = 0;
						var _g41 = columns.length;
						while(_g51 < _g41) {
							var j1 = _g51++;
							var val1 = Reflect.field(row,columns[j1]);
							output.setCell(j1,i,coopy.Coopy.cellFor(val1));
						}
					}
				}
			}
			if(output != null) output.trimBlank();
			return output;
		}
		,runDiff: function(parent,a,b,flags,output) {
			var ct = coopy.Coopy.compareTables3(parent,a,b,flags);
			var align = ct.align();
			var td = new coopy.TableDiff(align,flags);
			var o = new coopy.SimpleTable(0,0);
			var os = new coopy.Tables(o);
			td.hiliteWithNesting(os);
			var use_color = flags.terminal_format == "ansi";
			if(flags.terminal_format == null) {
				if((output == null || output == "-") && (this.output_format == "copy" || this.output_format == "csv")) {
					if(this.io != null) {
						if(this.io.isTtyKnown()) use_color = this.io.isTty();
					}
				}
			}
			this.saveTables(output,os,use_color);
		}
		,loadTable: function(name) {
			var ext = this.checkFormat(name);
			if(ext == "sqlite") {
				var sql = this.io.openSqliteDatabase(name);
				if(sql == null) {
					this.io.writeStderr("! Cannot open database, aborting\n");
					return null;
				}
				var tab = new coopy.SqlTables(sql,this.flags);
				return tab;
			}
			var txt = this.io.getContent(name);
			if(ext == "ndjson") {
				var t = new coopy.SimpleTable(0,0);
				var ndjson = new coopy.Ndjson(t);
				ndjson.parse(txt);
				return t;
			}
			if(ext == "json" || ext == "") try {
				var json = new haxe.format.JsonParser(txt).parseRec();
				this.format_preference = "json";
				var t1 = this.jsonToTables(json);
				if(t1 == null) throw "JSON failed";
				return t1;
			} catch( e ) {
				if(ext == "json") throw e;
			}
			this.format_preference = "csv";
			var csv = new coopy.Csv(this.delim_preference);
			var output = new coopy.SimpleTable(0,0);
			csv.parseTable(txt,output);
			if(output != null) output.trimBlank();
			return output;
		}
		,command: function(io,cmd,args) {
			var r = 0;
			if(io.async()) r = io.command(cmd,args);
			if(r != 999) {
				io.writeStdout("$ " + cmd);
				var _g = 0;
				while(_g < args.length) {
					var arg = args[_g];
					++_g;
					io.writeStdout(" ");
					var spaced = arg.indexOf(" ") >= 0;
					if(spaced) io.writeStdout("\"");
					io.writeStdout(arg);
					if(spaced) io.writeStdout("\"");
				}
				io.writeStdout("\n");
			}
			if(!io.async()) r = io.command(cmd,args);
			return r;
		}
		,installGitDriver: function(io,formats) {
			var r = 0;
			if(this.status == null) {
				this.status = new haxe.ds.StringMap();
				this.daff_cmd = "";
			}
			var key = "hello";
			if(!this.status.exists(key)) {
				io.writeStdout("Setting up git to use daff on");
				var _g = 0;
				while(_g < formats.length) {
					var format = formats[_g];
					++_g;
					io.writeStdout(" *." + format);
				}
				io.writeStdout(" files\n");
				this.status.set(key,r);
			}
			key = "can_run_git";
			if(!this.status.exists(key)) {
				r = this.command(io,"git",["--version"]);
				if(r == 999) return r;
				this.status.set(key,r);
				if(r != 0) {
					io.writeStderr("! Cannot run git, aborting\n");
					return 1;
				}
				io.writeStdout("- Can run git\n");
			}
			var daffs = ["daff","daff.rb","daff.py"];
			if(this.daff_cmd == "") {
				var _g1 = 0;
				while(_g1 < daffs.length) {
					var daff = daffs[_g1];
					++_g1;
					var key1 = "can_run_" + daff;
					if(!this.status.exists(key1)) {
						r = this.command(io,daff,["version"]);
						if(r == 999) return r;
						this.status.set(key1,r);
						if(r == 0) {
							this.daff_cmd = daff;
							io.writeStdout("- Can run " + daff + " as \"" + daff + "\"\n");
							break;
						}
					}
				}
				if(this.daff_cmd == "") {
					io.writeStderr("! Cannot find daff, is it in your path?\n");
					return 1;
				}
			}
			var _g2 = 0;
			while(_g2 < formats.length) {
				var format1 = formats[_g2];
				++_g2;
				key = "have_diff_driver_" + format1;
				if(!this.status.exists(key)) {
					r = this.command(io,"git",["config","--global","--get","diff.daff-" + format1 + ".command"]);
					if(r == 999) return r;
					this.status.set(key,r);
				}
				var have_diff_driver = this.status.get(key) == 0;
				key = "add_diff_driver_" + format1;
				if(!this.status.exists(key)) {
					r = this.command(io,"git",["config","--global","diff.daff-" + format1 + ".command",this.daff_cmd + " diff --git"]);
					if(r == 999) return r;
					if(have_diff_driver) io.writeStdout("- Cleared existing daff diff driver for " + format1 + "\n");
					io.writeStdout("- Added diff driver for " + format1 + "\n");
					this.status.set(key,r);
				}
				key = "have_merge_driver_" + format1;
				if(!this.status.exists(key)) {
					r = this.command(io,"git",["config","--global","--get","merge.daff-" + format1 + ".driver"]);
					if(r == 999) return r;
					this.status.set(key,r);
				}
				var have_merge_driver = this.status.get(key) == 0;
				key = "name_merge_driver_" + format1;
				if(!this.status.exists(key)) {
					if(!have_merge_driver) {
						r = this.command(io,"git",["config","--global","merge.daff-" + format1 + ".name","daff tabular " + format1 + " merge"]);
						if(r == 999) return r;
					} else r = 0;
					this.status.set(key,r);
				}
				key = "add_merge_driver_" + format1;
				if(!this.status.exists(key)) {
					r = this.command(io,"git",["config","--global","merge.daff-" + format1 + ".driver",this.daff_cmd + " merge --output %A %O %A %B"]);
					if(r == 999) return r;
					if(have_merge_driver) io.writeStdout("- Cleared existing daff merge driver for " + format1 + "\n");
					io.writeStdout("- Added merge driver for " + format1 + "\n");
					this.status.set(key,r);
				}
			}
			if(!io.exists(".git/config")) {
				io.writeStderr("! This next part needs to happen in a git repository.\n");
				io.writeStderr("! Please run again from the root of a git repository.\n");
				return 1;
			}
			var attr = ".gitattributes";
			var txt = "";
			var post = "";
			if(!io.exists(attr)) io.writeStdout("- No .gitattributes file\n"); else {
				io.writeStdout("- You have a .gitattributes file\n");
				txt = io.getContent(attr);
			}
			var need_update = false;
			var _g3 = 0;
			while(_g3 < formats.length) {
				var format2 = formats[_g3];
				++_g3;
				if(txt.indexOf("*." + format2) >= 0) io.writeStderr("- Your .gitattributes file already mentions *." + format2 + "\n"); else {
					post += "*." + format2 + " diff=daff-" + format2 + "\n";
					post += "*." + format2 + " merge=daff-" + format2 + "\n";
					io.writeStdout("- Placing the following lines in .gitattributes:\n");
					io.writeStdout(post);
					if(txt != "" && !need_update) txt += "\n";
					txt += post;
					need_update = true;
				}
			}
			if(need_update) io.saveContent(attr,txt);
			io.writeStdout("- Done!\n");
			return 0;
		}
		,run: function(args,io) {
			if(io == null) {
				console.log("No system interface available");
				return 1;
			}
			this.init();
			this.io = io;
			var more = true;
			var output = null;
			var inplace = false;
			var git = false;
			this.flags = new coopy.CompareFlags();
			this.flags.always_show_header = true;
			while(more) {
				more = false;
				var _g1 = 0;
				var _g = args.length;
				while(_g1 < _g) {
					var i = _g1++;
					var tag = args[i];
					if(tag == "--output") {
						more = true;
						output = args[i + 1];
						args.splice(i,2);
						break;
					} else if(tag == "--css") {
						more = true;
						this.fragment = true;
						this.css_output = args[i + 1];
						args.splice(i,2);
						break;
					} else if(tag == "--fragment") {
						more = true;
						this.fragment = true;
						args.splice(i,1);
						break;
					} else if(tag == "--plain") {
						more = true;
						this.pretty = false;
						args.splice(i,1);
						break;
					} else if(tag == "--all") {
						more = true;
						this.flags.show_unchanged = true;
						this.flags.show_unchanged_columns = true;
						args.splice(i,1);
						break;
					} else if(tag == "--all-rows") {
						more = true;
						this.flags.show_unchanged = true;
						args.splice(i,1);
						break;
					} else if(tag == "--all-columns") {
						more = true;
						this.flags.show_unchanged_columns = true;
						args.splice(i,1);
						break;
					} else if(tag == "--act") {
						more = true;
						if(this.flags.acts == null) this.flags.acts = new haxe.ds.StringMap();
						{
							this.flags.acts.set(args[i + 1],true);
							true;
						}
						args.splice(i,2);
						break;
					} else if(tag == "--context") {
						more = true;
						var context = Std.parseInt(args[i + 1]);
						if(context >= 0) this.flags.unchanged_context = context;
						args.splice(i,2);
						break;
					} else if(tag == "--inplace") {
						more = true;
						inplace = true;
						args.splice(i,1);
						break;
					} else if(tag == "--git") {
						more = true;
						git = true;
						args.splice(i,1);
						break;
					} else if(tag == "--unordered") {
						more = true;
						this.flags.ordered = false;
						this.flags.unchanged_context = 0;
						this.order_set = true;
						args.splice(i,1);
						break;
					} else if(tag == "--ordered") {
						more = true;
						this.flags.ordered = true;
						this.order_set = true;
						args.splice(i,1);
						break;
					} else if(tag == "--color") {
						more = true;
						this.flags.terminal_format = "ansi";
						args.splice(i,1);
						break;
					} else if(tag == "--no-color") {
						more = true;
						this.flags.terminal_format = "plain";
						args.splice(i,1);
						break;
					} else if(tag == "--input-format") {
						more = true;
						this.setFormat(args[i + 1]);
						args.splice(i,2);
						break;
					} else if(tag == "--output-format") {
						more = true;
						this.output_format = args[i + 1];
						this.output_format_set = true;
						args.splice(i,2);
						break;
					} else if(tag == "--id") {
						more = true;
						if(this.flags.ids == null) this.flags.ids = [];
						this.flags.ids.push(args[i + 1]);
						args.splice(i,2);
						break;
					} else if(tag == "--ignore") {
						more = true;
						this.flags.ignoreColumn(args[i + 1]);
						args.splice(i,2);
						break;
					} else if(tag == "--index") {
						more = true;
						this.flags.always_show_order = true;
						this.flags.never_show_order = false;
						args.splice(i,1);
						break;
					} else if(tag == "--www") {
						more = true;
						this.output_format = "www";
						this.output_format_set = true;
						args.splice(i,1);
					} else if(tag == "--table") {
						more = true;
						this.flags.addTable(args[i + 1]);
						args.splice(i,2);
						break;
					} else if(tag == "-w" || tag == "--ignore-whitespace") {
						more = true;
						this.flags.ignore_whitespace = true;
						args.splice(i,1);
						break;
					} else if(tag == "-i" || tag == "--ignore-case") {
						more = true;
						this.flags.ignore_case = true;
						args.splice(i,1);
						break;
					} else if(tag == "--padding") {
						more = true;
						this.flags.padding_strategy = args[i + 1];
						args.splice(i,2);
						break;
					}
				}
			}
			var cmd = args[0];
			if(args.length < 2) {
				if(cmd == "version") {
					io.writeStdout(coopy.Coopy.VERSION + "\n");
					return 0;
				}
				if(cmd == "git") {
					io.writeStdout("You can use daff to improve git's handling of csv files, by using it as a\ndiff driver (for showing what has changed) and as a merge driver (for merging\nchanges between multiple versions).\n");
					io.writeStdout("\n");
					io.writeStdout("Automatic setup\n");
					io.writeStdout("---------------\n\n");
					io.writeStdout("Run:\n");
					io.writeStdout("  daff git csv\n");
					io.writeStdout("\n");
					io.writeStdout("Manual setup\n");
					io.writeStdout("------------\n\n");
					io.writeStdout("Create and add a file called .gitattributes in the root directory of your\nrepository, containing:\n\n");
					io.writeStdout("  *.csv diff=daff-csv\n");
					io.writeStdout("  *.csv merge=daff-csv\n");
					io.writeStdout("\nCreate a file called .gitconfig in your home directory (or alternatively\nopen .git/config for a particular repository) and add:\n\n");
					io.writeStdout("  [diff \"daff-csv\"]\n");
					io.writeStdout("  command = daff diff --git\n");
					io.writeStderr("\n");
					io.writeStdout("  [merge \"daff-csv\"]\n");
					io.writeStdout("  name = daff tabular merge\n");
					io.writeStdout("  driver = daff merge --output %A %O %A %B\n\n");
					io.writeStderr("Make sure you can run daff from the command-line as just \"daff\" - if not,\nreplace \"daff\" in the driver and command lines above with the correct way\nto call it. Add --no-color if your terminal does not support ANSI colors.");
					io.writeStderr("\n");
					return 0;
				}
				io.writeStderr("daff can produce and apply tabular diffs.\n");
				io.writeStderr("Call as:\n");
				io.writeStderr("  daff [--color] [--no-color] [--output OUTPUT.csv] a.csv b.csv\n");
				io.writeStderr("  daff [--output OUTPUT.html] a.csv b.csv\n");
				io.writeStderr("  daff [--output OUTPUT.csv] parent.csv a.csv b.csv\n");
				io.writeStderr("  daff [--output OUTPUT.ndjson] a.ndjson b.ndjson\n");
				io.writeStderr("  daff [--www] a.csv b.csv\n");
				io.writeStderr("  daff patch [--inplace] [--output OUTPUT.csv] a.csv patch.csv\n");
				io.writeStderr("  daff merge [--inplace] [--output OUTPUT.csv] parent.csv a.csv b.csv\n");
				io.writeStderr("  daff trim [--output OUTPUT.csv] source.csv\n");
				io.writeStderr("  daff render [--output OUTPUT.html] diff.csv\n");
				io.writeStderr("  daff copy in.csv out.tsv\n");
				io.writeStderr("  daff git\n");
				io.writeStderr("  daff version\n");
				io.writeStderr("\n");
				io.writeStderr("The --inplace option to patch and merge will result in modification of a.csv.\n");
				io.writeStderr("\n");
				io.writeStderr("If you need more control, here is the full list of flags:\n");
				io.writeStderr("  daff diff [--output OUTPUT.csv] [--context NUM] [--all] [--act ACT] a.csv b.csv\n");
				io.writeStderr("     --act ACT:     show only a certain kind of change (update, insert, delete)\n");
				io.writeStderr("     --all:         do not prune unchanged rows or columns\n");
				io.writeStderr("     --all-rows:    do not prune unchanged rows\n");
				io.writeStderr("     --all-columns: do not prune unchanged columns\n");
				io.writeStderr("     --color:       highlight changes with terminal colors (default in terminals)\n");
				io.writeStderr("     --context NUM: show NUM rows of context\n");
				io.writeStderr("     --id:          specify column to use as primary key (repeat for multi-column key)\n");
				io.writeStderr("     --ignore:      specify column to ignore completely (can repeat)\n");
				io.writeStderr("     --index:       include row/columns numbers from original tables\n");
				io.writeStderr("     --input-format [csv|tsv|ssv|json]: set format to expect for input\n");
				io.writeStderr("     --no-color:    make sure terminal colors are not used\n");
				io.writeStderr("     --ordered:     assume row order is meaningful (default for CSV)\n");
				io.writeStderr("     --output-format [csv|tsv|ssv|json|copy|html]: set format for output\n");
				io.writeStderr("     --padding [dense|sparse|smart]: set padding method for aligning columns\n");
				io.writeStderr("     --table NAME:  compare the named table, used with SQL sources\n");
				io.writeStderr("     --unordered:   assume row order is meaningless (default for json formats)\n");
				io.writeStderr("     -w / --ignore-whitespace: ignore changes in leading/trailing whitespace\n");
				io.writeStderr("     -i / --ignore-case: ignore differences in case\n");
				io.writeStderr("\n");
				io.writeStderr("  daff render [--output OUTPUT.html] [--css CSS.css] [--fragment] [--plain] diff.csv\n");
				io.writeStderr("     --css CSS.css: generate a suitable css file to go with the html\n");
				io.writeStderr("     --fragment:    generate just a html fragment rather than a page\n");
				io.writeStderr("     --plain:       do not use fancy utf8 characters to make arrows prettier\n");
				io.writeStderr("     --www:         send output to a browser\n");
				return 1;
			}
			var cmd1 = args[0];
			var offset = 1;
			if(!Lambda.has(["diff","patch","merge","trim","render","git","version","copy"],cmd1)) {
				if(cmd1.indexOf(".") != -1 || cmd1.indexOf("--") == 0) {
					cmd1 = "diff";
					offset = 0;
				}
			}
			if(cmd1 == "git") {
				var types = args.splice(offset,args.length - offset);
				return this.installGitDriver(io,types);
			}
			if(git) {
				var ct = args.length - offset;
				if(ct != 7 && ct != 9) {
					io.writeStderr("Expected 7 or 9 parameters from git, but got " + ct + "\n");
					return 1;
				}
				var git_args = args.splice(offset,ct);
				args.splice(0,args.length);
				offset = 0;
				var old_display_path = git_args[0];
				var new_display_path = git_args[0];
				var old_file = git_args[1];
				var new_file = git_args[4];
				if(ct == 9) {
					io.writeStdout(git_args[8]);
					new_display_path = git_args[7];
				}
				io.writeStdout("--- a/" + old_display_path + "\n");
				io.writeStdout("+++ b/" + new_display_path + "\n");
				args.push(old_file);
				args.push(new_file);
			}
			var parent = null;
			if(args.length - offset >= 3) {
				parent = this.loadTable(args[offset]);
				offset++;
			}
			var aname = args[offset];
			var a = this.loadTable(aname);
			var b = null;
			if(args.length - offset >= 2) {
				if(cmd1 != "copy") b = this.loadTable(args[1 + offset]); else output = args[1 + offset];
			}
			this.flags.diff_strategy = this.strategy;
			if(inplace) {
				if(output != null) io.writeStderr("Please do not use --inplace when specifying an output.\n");
				output = aname;
				return 1;
			}
			if(output == null) output = "-";
			var ok = true;
			if(cmd1 == "diff") {
				if(!this.order_set) {
					this.flags.ordered = this.order_preference;
					if(!this.flags.ordered) this.flags.unchanged_context = 0;
				}
				this.flags.allow_nested_cells = this.nested_output;
				this.runDiff(parent,a,b,this.flags,output);
			} else if(cmd1 == "patch") {
				var patcher = new coopy.HighlightPatch(a,b);
				patcher.apply();
				this.saveTable(output,a);
			} else if(cmd1 == "merge") {
				var merger = new coopy.Merger(parent,a,b,this.flags);
				var conflicts = merger.apply();
				ok = conflicts == 0;
				if(conflicts > 0) io.writeStderr(conflicts + " conflict" + (conflicts > 1?"s":"") + "\n");
				this.saveTable(output,a);
			} else if(cmd1 == "trim") this.saveTable(output,a); else if(cmd1 == "render") this.renderTable(output,a); else if(cmd1 == "copy") this.saveTable(output,a);
			if(ok) return 0; else return 1;
		}
		,coopyhx: function(io) {
			var args = io.args();
			if(args[0] == "--keep") return coopy.Coopy.keepAround();
			return this.run(args,io);
		}
		,__class__: coopy.Coopy
	};
	coopy.CrossMatch = function() {
	};
	coopy.CrossMatch.__name__ = true;
	coopy.CrossMatch.prototype = {
		__class__: coopy.CrossMatch
	};
	coopy.Csv = $hx_exports.coopy.Csv = function(delim) {
		if(delim == null) delim = ",";
		this.cursor = 0;
		this.row_ended = false;
		if(delim == null) this.delim = ","; else this.delim = delim;
	};
	coopy.Csv.__name__ = true;
	coopy.Csv.prototype = {
		renderTable: function(t) {
			var result = "";
			var txt = "";
			var v = t.getCellView();
			var stream = new coopy.TableStream(t);
			var w = stream.width();
			while(stream.fetch()) {
				var _g = 0;
				while(_g < w) {
					var x = _g++;
					if(x > 0) txt += this.delim;
					txt += this.renderCell(v,stream.getCell(x));
				}
				txt += "\r\n";
			}
			return txt;
		}
		,renderCell: function(v,d) {
			if(d == null) return "NULL";
			var str = v.toString(d);
			var need_quote = false;
			var _g1 = 0;
			var _g = str.length;
			while(_g1 < _g) {
				var i = _g1++;
				var ch = str.charAt(i);
				if(ch == "\"" || ch == "'" || ch == this.delim || ch == "\r" || ch == "\n" || ch == "\t" || ch == " ") {
					need_quote = true;
					break;
				}
			}
			var result = "";
			if(need_quote) result += "\"";
			var line_buf = "";
			var _g11 = 0;
			var _g2 = str.length;
			while(_g11 < _g2) {
				var i1 = _g11++;
				var ch1 = str.charAt(i1);
				if(ch1 == "\"") result += "\"";
				if(ch1 != "\r" && ch1 != "\n") {
					if(line_buf.length > 0) {
						result += line_buf;
						line_buf = "";
					}
					result += ch1;
				} else line_buf += ch1;
			}
			if(need_quote) result += "\"";
			return result;
		}
		,parseTable: function(txt,tab) {
			if(!tab.isResizable()) return false;
			this.cursor = 0;
			this.row_ended = false;
			this.has_structure = true;
			tab.resize(0,0);
			var w = 0;
			var h = 0;
			var at = 0;
			var yat = 0;
			while(this.cursor < txt.length) {
				var cell = this.parseCellPart(txt);
				if(yat >= h) {
					h = yat + 1;
					tab.resize(w,h);
				}
				if(at >= w) {
					if(yat > 0) {
						if(cell != "" && cell != null) {
							var context = "";
							var _g = 0;
							while(_g < w) {
								var i = _g++;
								if(i > 0) context += ",";
								context += Std.string(tab.getCell(i,yat));
							}
							console.log("Ignored overflowing row " + yat + " with cell '" + cell + "' after: " + context);
						}
					} else {
						w = at + 1;
						tab.resize(w,h);
					}
				}
				tab.setCell(at,h - 1,cell);
				at++;
				if(this.row_ended) {
					at = 0;
					yat++;
				}
				this.cursor++;
			}
			return true;
		}
		,makeTable: function(txt) {
			var tab = new coopy.SimpleTable(0,0);
			this.parseTable(txt,tab);
			return tab;
		}
		,parseCellPart: function(txt) {
			if(txt == null) return null;
			this.row_ended = false;
			var first_non_underscore = txt.length;
			var last_processed = 0;
			var quoting = false;
			var quote = 0;
			var result = "";
			var start = this.cursor;
			var _g1 = this.cursor;
			var _g = txt.length;
			while(_g1 < _g) {
				var i = _g1++;
				var ch = HxOverrides.cca(txt,i);
				last_processed = i;
				if(ch != 95 && i < first_non_underscore) first_non_underscore = i;
				if(this.has_structure) {
					if(!quoting) {
						if(ch == HxOverrides.cca(this.delim,0)) break;
						if(ch == 13 || ch == 10) {
							var ch2 = HxOverrides.cca(txt,i + 1);
							if(ch2 != null) {
								if(ch2 != ch) {
									if(ch2 == 13 || ch2 == 10) last_processed++;
								}
							}
							this.row_ended = true;
							break;
						}
						if(ch == 34) {
							if(i == this.cursor) {
								quoting = true;
								quote = ch;
								if(i != start) result += String.fromCharCode(ch);
								continue;
							} else if(ch == quote) quoting = true;
						}
						result += String.fromCharCode(ch);
						continue;
					}
					if(ch == quote) {
						quoting = false;
						continue;
					}
				}
				result += String.fromCharCode(ch);
			}
			this.cursor = last_processed;
			if(quote == 0) {
				if(result == "NULL") return null;
				if(first_non_underscore > start) {
					var del = first_non_underscore - start;
					if(HxOverrides.substr(result,del,null) == "NULL") return HxOverrides.substr(result,1,null);
				}
			}
			return result;
		}
		,parseCell: function(txt) {
			this.cursor = 0;
			this.row_ended = false;
			this.has_structure = false;
			return this.parseCellPart(txt);
		}
		,__class__: coopy.Csv
	};
	coopy.DiffRender = $hx_exports.coopy.DiffRender = function() {
		this.text_to_insert = [];
		this.open = false;
		this.pretty_arrows = true;
	};
	coopy.DiffRender.__name__ = true;
	coopy.DiffRender.examineCell = function(x,y,view,raw,vcol,vrow,vcorner,cell,offset) {
		if(offset == null) offset = 0;
		var nested = view.isHash(raw);
		cell.category = "";
		cell.category_given_tr = "";
		cell.separator = "";
		cell.pretty_separator = "";
		cell.conflicted = false;
		cell.updated = false;
		cell.meta = cell.pvalue = cell.lvalue = cell.rvalue = null;
		cell.value = raw;
		cell.pretty_value = cell.value;
		if(vrow == null) vrow = "";
		if(vcol == null) vcol = "";
		if(vrow.length >= 3 && vrow.charAt(0) == "@" && vrow.charAt(1) != "@") {
			var idx = vrow.indexOf("@",1);
			if(idx >= 0) {
				cell.meta = HxOverrides.substr(vrow,1,idx - 1);
				vrow = HxOverrides.substr(vrow,idx + 1,vrow.length);
				cell.category = "meta";
			}
		}
		var removed_column = false;
		if(vrow == ":") cell.category = "move";
		if(vrow == "" && offset == 1 && y == 0) cell.category = "index";
		if(vcol.indexOf("+++") >= 0) cell.category_given_tr = cell.category = "add"; else if(vcol.indexOf("---") >= 0) {
			cell.category_given_tr = cell.category = "remove";
			removed_column = true;
		}
		if(vrow == "!") cell.category = "spec"; else if(vrow == "@@") cell.category = "header"; else if(vrow == "...") cell.category = "gap"; else if(vrow == "+++") {
			if(!removed_column) cell.category = "add";
		} else if(vrow == "---") cell.category = "remove"; else if(vrow.indexOf("->") >= 0) {
			if(!removed_column) {
				var tokens = vrow.split("!");
				var full = vrow;
				var part = tokens[1];
				if(part == null) part = full;
				var str = view.toString(cell.value);
				if(str == null) str = "";
				if(nested || str.indexOf(part) >= 0) {
					var cat = "modify";
					var div = part;
					if(part != full) {
						if(nested) cell.conflicted = view.hashExists(raw,"theirs"); else cell.conflicted = str.indexOf(full) >= 0;
						if(cell.conflicted) {
							div = full;
							cat = "conflict";
						}
					}
					cell.updated = true;
					cell.separator = div;
					cell.pretty_separator = div;
					if(nested) {
						if(cell.conflicted) tokens = [view.hashGet(raw,"before"),view.hashGet(raw,"ours"),view.hashGet(raw,"theirs")]; else tokens = [view.hashGet(raw,"before"),view.hashGet(raw,"after")];
					} else {
						cell.pretty_value = view.toString(cell.pretty_value);
						if(cell.pretty_value == null) cell.pretty_value = "";
						if(cell.pretty_value == div) tokens = ["",""]; else tokens = cell.pretty_value.split(div);
					}
					var pretty_tokens = tokens;
					if(tokens.length >= 2) {
						pretty_tokens[0] = coopy.DiffRender.markSpaces(tokens[0],tokens[1]);
						pretty_tokens[1] = coopy.DiffRender.markSpaces(tokens[1],tokens[0]);
					}
					if(tokens.length >= 3) {
						var ref = pretty_tokens[0];
						pretty_tokens[0] = coopy.DiffRender.markSpaces(ref,tokens[2]);
						pretty_tokens[2] = coopy.DiffRender.markSpaces(tokens[2],ref);
					}
					cell.pretty_separator = String.fromCharCode(8594);
					cell.pretty_value = pretty_tokens.join(cell.pretty_separator);
					cell.category_given_tr = cell.category = cat;
					var offset1;
					if(cell.conflicted) offset1 = 1; else offset1 = 0;
					cell.lvalue = tokens[offset1];
					cell.rvalue = tokens[offset1 + 1];
					if(cell.conflicted) cell.pvalue = tokens[0];
				}
			}
		}
		if(x == 0 && offset > 0) cell.category_given_tr = cell.category = "index";
	};
	coopy.DiffRender.markSpaces = function(sl,sr) {
		if(sl == sr) return sl;
		if(sl == null || sr == null) return sl;
		var slc = StringTools.replace(sl," ","");
		var src = StringTools.replace(sr," ","");
		if(slc != src) return sl;
		var slo = new String("");
		var il = 0;
		var ir = 0;
		while(il < sl.length) {
			var cl = sl.charAt(il);
			var cr = "";
			if(ir < sr.length) cr = sr.charAt(ir);
			if(cl == cr) {
				slo += cl;
				il++;
				ir++;
			} else if(cr == " ") ir++; else {
				slo += String.fromCharCode(9251);
				il++;
			}
		}
		return slo;
	};
	coopy.DiffRender.renderCell = function(tab,view,x,y) {
		var cell = new coopy.CellInfo();
		var corner = view.toString(tab.getCell(0,0));
		var off;
		if(corner == "@:@") off = 1; else off = 0;
		coopy.DiffRender.examineCell(x,y,view,tab.getCell(x,y),view.toString(tab.getCell(x,off)),view.toString(tab.getCell(off,y)),corner,cell,off);
		return cell;
	};
	coopy.DiffRender.prototype = {
		usePrettyArrows: function(flag) {
			this.pretty_arrows = flag;
		}
		,insert: function(str) {
			this.text_to_insert.push(str);
		}
		,beginTable: function() {
			this.insert("<table>\n");
			this.section = null;
		}
		,setSection: function(str) {
			if(str == this.section) return;
			if(this.section != null) {
				this.insert("</t");
				this.insert(this.section);
				this.insert(">\n");
			}
			this.section = str;
			if(this.section != null) {
				this.insert("<t");
				this.insert(this.section);
				this.insert(">\n");
			}
		}
		,beginRow: function(mode) {
			this.td_open = "<td";
			this.td_close = "</td>";
			var row_class = "";
			if(mode == "header") {
				this.td_open = "<th";
				this.td_close = "</th>";
			}
			row_class = mode;
			var tr = "<tr>";
			if(row_class != "") tr = "<tr class=\"" + row_class + "\">";
			this.insert(tr);
		}
		,insertCell: function(txt,mode) {
			var cell_decorate = "";
			if(mode != "") cell_decorate = " class=\"" + mode + "\"";
			this.insert(this.td_open + cell_decorate + ">");
			if(txt != null) this.insert(txt); else this.insert("null");
			this.insert(this.td_close);
		}
		,endRow: function() {
			this.insert("</tr>\n");
		}
		,endTable: function() {
			this.setSection(null);
			this.insert("</table>\n");
		}
		,html: function() {
			return this.text_to_insert.join("");
		}
		,toString: function() {
			return this.html();
		}
		,render: function(tab) {
			tab = coopy.Coopy.tablify(tab);
			if(tab.get_width() == 0 || tab.get_height() == 0) return this;
			var render = this;
			render.beginTable();
			var change_row = -1;
			var cell = new coopy.CellInfo();
			var view = tab.getCellView();
			var corner = view.toString(tab.getCell(0,0));
			var off;
			if(corner == "@:@") off = 1; else off = 0;
			if(off > 0) {
				if(tab.get_width() <= 1 || tab.get_height() <= 1) return this;
			}
			var _g1 = 0;
			var _g = tab.get_height();
			while(_g1 < _g) {
				var row = _g1++;
				var open = false;
				var txt = view.toString(tab.getCell(off,row));
				if(txt == null) txt = "";
				coopy.DiffRender.examineCell(off,row,view,txt,"",txt,corner,cell,off);
				var row_mode = cell.category;
				if(row_mode == "spec") change_row = row;
				if(row_mode == "header" || row_mode == "spec" || row_mode == "index" || row_mode == "meta") this.setSection("head"); else this.setSection("body");
				render.beginRow(row_mode);
				var _g3 = 0;
				var _g2 = tab.get_width();
				while(_g3 < _g2) {
					var c = _g3++;
					coopy.DiffRender.examineCell(c,row,view,tab.getCell(c,row),change_row >= 0?view.toString(tab.getCell(c,change_row)):"",txt,corner,cell,off);
					render.insertCell(this.pretty_arrows?cell.pretty_value:cell.value,cell.category_given_tr);
				}
				render.endRow();
			}
			render.endTable();
			return this;
		}
		,renderTables: function(tabs) {
			var order = tabs.getOrder();
			if(order.length == 0 || tabs.hasInsDel()) this.render(tabs.one());
			var _g1 = 1;
			var _g = order.length;
			while(_g1 < _g) {
				var i = _g1++;
				var name = order[i];
				var tab = tabs.get(name);
				if(tab.get_height() <= 1) continue;
				this.insert("<h3>");
				this.insert(name);
				this.insert("</h3>\n");
				this.render(tab);
			}
			return this;
		}
		,sampleCss: function() {
			return ".highlighter .add { \n  background-color: #7fff7f;\n}\n\n.highlighter .remove { \n  background-color: #ff7f7f;\n}\n\n.highlighter td.modify { \n  background-color: #7f7fff;\n}\n\n.highlighter td.conflict { \n  background-color: #f00;\n}\n\n.highlighter .spec { \n  background-color: #aaa;\n}\n\n.highlighter .move { \n  background-color: #ffa;\n}\n\n.highlighter .null { \n  color: #888;\n}\n\n.highlighter table { \n  border-collapse:collapse;\n}\n\n.highlighter td, .highlighter th {\n  border: 1px solid #2D4068;\n  padding: 3px 7px 2px;\n}\n\n.highlighter th, .highlighter .header, .highlighter .meta {\n  background-color: #aaf;\n  font-weight: bold;\n  padding-bottom: 4px;\n  padding-top: 5px;\n  text-align:left;\n}\n\n.highlighter tr.header th {\n  border-bottom: 2px solid black;\n}\n\n.highlighter tr.index td, .highlighter .index, .highlighter tr.header th.index {\n  background-color: white;\n  border: none;\n}\n\n.highlighter .gap {\n  color: #888;\n}\n\n.highlighter td {\n  empty-cells: show;\n}\n";
		}
		,completeHtml: function() {
			this.text_to_insert.splice(0,0,"<!DOCTYPE html>\n<html>\n<head>\n<meta charset='utf-8'>\n<style TYPE='text/css'>\n");
			var x = this.sampleCss();
			this.text_to_insert.splice(1,0,x);
			this.text_to_insert.splice(2,0,"</style>\n</head>\n<body>\n<div class='highlighter'>\n");
			this.text_to_insert.push("</div>\n</body>\n</html>\n");
		}
		,__class__: coopy.DiffRender
	};
	coopy.FlatCellBuilder = function(flags) {
		this.flags = flags;
	};
	coopy.FlatCellBuilder.__name__ = true;
	coopy.FlatCellBuilder.__interfaces__ = [coopy.CellBuilder];
	coopy.FlatCellBuilder.quoteForDiff = function(v,d) {
		var nil = "NULL";
		if(v.equals(d,null)) return nil;
		var str = v.toString(d);
		var score = 0;
		var _g1 = 0;
		var _g = str.length;
		while(_g1 < _g) {
			var i = _g1++;
			if(HxOverrides.cca(str,score) != 95) break;
			score++;
		}
		if(HxOverrides.substr(str,score,null) == nil) str = "_" + str;
		return str;
	};
	coopy.FlatCellBuilder.prototype = {
		needSeparator: function() {
			return true;
		}
		,setSeparator: function(separator) {
			this.separator = separator;
		}
		,setConflictSeparator: function(separator) {
			this.conflict_separator = separator;
		}
		,setView: function(view) {
			this.view = view;
		}
		,update: function(local,remote) {
			return this.view.toDatum(coopy.FlatCellBuilder.quoteForDiff(this.view,local) + this.separator + coopy.FlatCellBuilder.quoteForDiff(this.view,remote));
		}
		,conflict: function(parent,local,remote) {
			return this.view.toString(parent) + this.conflict_separator + this.view.toString(local) + this.conflict_separator + this.view.toString(remote);
		}
		,marker: function(label) {
			return this.view.toDatum(label);
		}
		,links: function(unit,row_like) {
			if(this.flags.count_like_a_spreadsheet && !row_like) return this.view.toDatum(unit.toBase26String());
			return this.view.toDatum(unit.toString());
		}
		,__class__: coopy.FlatCellBuilder
	};
	coopy.Row = function() { };
	coopy.Row.__name__ = true;
	coopy.Row.prototype = {
		__class__: coopy.Row
	};
	coopy.HighlightPatch = $hx_exports.coopy.HighlightPatch = function(source,patch,flags) {
		this.source = source;
		this.patch = patch;
		this.flags = flags;
		if(flags == null) this.flags = new coopy.CompareFlags();
		this.view = patch.getCellView();
		this.sourceView = source.getCellView();
		this.meta = source.getMeta();
	};
	coopy.HighlightPatch.__name__ = true;
	coopy.HighlightPatch.__interfaces__ = [coopy.Row];
	coopy.HighlightPatch.prototype = {
		reset: function() {
			this.header = new haxe.ds.IntMap();
			this.headerPre = new haxe.ds.StringMap();
			this.headerPost = new haxe.ds.StringMap();
			this.headerRename = new haxe.ds.StringMap();
			this.headerMove = null;
			this.modifier = new haxe.ds.IntMap();
			this.mods = [];
			this.cmods = [];
			this.csv = new coopy.Csv();
			this.rcOffset = 0;
			this.currentRow = -1;
			this.rowInfo = new coopy.CellInfo();
			this.cellInfo = new coopy.CellInfo();
			this.sourceInPatchCol = this.patchInSourceCol = this.patchInDestCol = null;
			this.patchInSourceRow = new haxe.ds.IntMap();
			this.indexes = null;
			this.lastSourceRow = -1;
			this.actions = [];
			this.rowPermutation = null;
			this.rowPermutationRev = null;
			this.colPermutation = null;
			this.colPermutationRev = null;
			this.haveDroppedColumns = false;
			this.headerRow = 0;
			this.preambleRow = 0;
			this.meta_change = false;
			this.process_meta = false;
			this.prev_meta = null;
			this.next_meta = null;
			this.finished_columns = false;
		}
		,processMeta: function() {
			this.process_meta = true;
		}
		,apply: function() {
			this.reset();
			if(this.patch.get_width() < 2) return true;
			if(this.patch.get_height() < 1) return true;
			this.payloadCol = 1 + this.rcOffset;
			this.payloadTop = this.patch.get_width();
			var corner = this.patch.getCellView().toString(this.patch.getCell(0,0));
			if(corner == "@:@") this.rcOffset = 1; else this.rcOffset = 0;
			var _g1 = 0;
			var _g = this.patch.get_height();
			while(_g1 < _g) {
				var r = _g1++;
				var str = this.view.toString(this.patch.getCell(this.rcOffset,r));
				this.actions.push(str != null?str:"");
			}
			this.preambleRow = this.headerRow = this.rcOffset;
			var _g11 = 0;
			var _g2 = this.patch.get_height();
			while(_g11 < _g2) {
				var r1 = _g11++;
				this.applyRow(r1);
			}
			this.finishColumns();
			this.finishRows();
			return true;
		}
		,needSourceColumns: function() {
			if(this.sourceInPatchCol != null) return;
			this.sourceInPatchCol = new haxe.ds.IntMap();
			this.patchInSourceCol = new haxe.ds.IntMap();
			var av = this.source.getCellView();
			var _g1 = 0;
			var _g = this.source.get_width();
			while(_g1 < _g) {
				var i = _g1++;
				var name = av.toString(this.source.getCell(i,0));
				var at = this.headerPre.get(name);
				if(at == null) continue;
				this.sourceInPatchCol.h[i] = at;
				this.patchInSourceCol.h[at] = i;
			}
		}
		,needDestColumns: function() {
			if(this.patchInDestCol != null) return;
			this.patchInDestCol = new haxe.ds.IntMap();
			this.destInPatchCol = new haxe.ds.IntMap();
			var _g = 0;
			var _g1 = this.cmods;
			while(_g < _g1.length) {
				var cmod = _g1[_g];
				++_g;
				if(cmod.patchRow != -1) {
					this.patchInDestCol.h[cmod.patchRow] = cmod.destRow;
					this.destInPatchCol.h[cmod.destRow] = cmod.patchRow;
				}
			}
		}
		,needSourceIndex: function() {
			if(this.indexes != null) return;
			var state = new coopy.TableComparisonState();
			state.a = this.source;
			state.b = this.source;
			var comp = new coopy.CompareTable(state);
			comp.storeIndexes();
			comp.run();
			comp.align();
			this.indexes = comp.getIndexes();
			this.needSourceColumns();
		}
		,setMetaProp: function(target,column_name,prop_name,value) {
			if(column_name == null) return;
			if(prop_name == null) return;
			if(!(__map_reserved[column_name] != null?target.existsReserved(column_name):target.h.hasOwnProperty(column_name))) {
				var value1 = [];
				if(__map_reserved[column_name] != null) target.setReserved(column_name,value1); else target.h[column_name] = value1;
			}
			var change = new coopy.PropertyChange();
			change.prevName = prop_name;
			change.name = prop_name;
			if(value == "") value = null;
			change.val = value;
			(__map_reserved[column_name] != null?target.getReserved(column_name):target.h[column_name]).push(change);
		}
		,applyMetaRow: function(code) {
			this.needSourceColumns();
			var codes = code.split("@");
			var prop_name = "";
			if(codes.length > 1) prop_name = codes[codes.length - 2];
			if(codes.length > 0) code = codes[codes.length - 1];
			if(this.prev_meta == null) this.prev_meta = new haxe.ds.StringMap();
			if(this.next_meta == null) this.next_meta = new haxe.ds.StringMap();
			var _g1 = this.payloadCol;
			var _g = this.payloadTop;
			while(_g1 < _g) {
				var i = _g1++;
				var txt = this.getDatum(i);
				var idx_patch = i;
				var idx_src;
				if(this.patchInSourceCol.h.hasOwnProperty(idx_patch)) idx_src = this.patchInSourceCol.h[idx_patch]; else idx_src = -1;
				var prev_name = null;
				var name = null;
				if(idx_src != -1) prev_name = this.source.getCell(idx_src,0);
				if(this.header.h.hasOwnProperty(idx_patch)) name = this.header.h[idx_patch];
				coopy.DiffRender.examineCell(0,0,this.view,txt,"",code,"",this.cellInfo);
				if(this.cellInfo.updated) {
					this.setMetaProp(this.prev_meta,prev_name,prop_name,this.cellInfo.lvalue);
					this.setMetaProp(this.next_meta,name,prop_name,this.cellInfo.rvalue);
				} else {
					this.setMetaProp(this.prev_meta,prev_name,prop_name,this.cellInfo.value);
					this.setMetaProp(this.next_meta,name,prop_name,this.cellInfo.value);
				}
			}
		}
		,applyRow: function(r) {
			this.currentRow = r;
			var code = this.actions[r];
			var done = false;
			if(r == 0 && this.rcOffset > 0) done = true; else if(code == "@@") {
				this.preambleRow = this.headerRow = r;
				this.applyHeader();
				this.applyAction("@@");
				done = true;
			} else if(code == "!") {
				this.preambleRow = this.headerRow = r;
				this.applyMeta();
				done = true;
			} else if(code.indexOf("@") == 0) {
				this.flags.addWarning("cannot usefully apply diffs with metadata yet: '" + code + "'");
				this.preambleRow = r;
				this.applyMetaRow(code);
				if(this.process_meta) {
					var codes = code.split("@");
					if(codes.length > 0) code = codes[codes.length - 1];
				} else {
					this.meta_change = true;
					done = true;
				}
				this.meta_change = true;
				done = true;
			}
			if(this.process_meta) return;
			if(!done) {
				this.finishColumns();
				if(code == "+++") this.applyAction(code); else if(code == "---") this.applyAction(code); else if(code == "+" || code == ":") this.applyAction(code); else if(code.indexOf("->") >= 0) this.applyAction("->"); else this.lastSourceRow = -1;
			}
		}
		,getDatum: function(c) {
			return this.patch.getCell(c,this.currentRow);
		}
		,getString: function(c) {
			return this.view.toString(this.getDatum(c));
		}
		,getStringNull: function(c) {
			var d = this.getDatum(c);
			if(d == null) return null;
			return this.view.toString(d);
		}
		,applyMeta: function() {
			var _g1 = this.payloadCol;
			var _g = this.payloadTop;
			while(_g1 < _g) {
				var i = _g1++;
				var name = this.getString(i);
				if(name == "") continue;
				this.modifier.h[i] = name;
			}
		}
		,applyHeader: function() {
			var _g1 = this.payloadCol;
			var _g = this.payloadTop;
			while(_g1 < _g) {
				var i = _g1++;
				var name = this.getString(i);
				if(name == "...") {
					this.modifier.h[i] = "...";
					this.haveDroppedColumns = true;
					continue;
				}
				var mod = this.modifier.h[i];
				var move = false;
				if(mod != null) {
					if(HxOverrides.cca(mod,0) == 58) {
						move = true;
						mod = HxOverrides.substr(mod,1,mod.length);
					}
				}
				this.header.h[i] = name;
				if(mod != null) {
					if(HxOverrides.cca(mod,0) == 40) {
						var prev_name = HxOverrides.substr(mod,1,mod.length - 2);
						this.headerPre.set(prev_name,i);
						this.headerPost.set(name,i);
						this.headerRename.set(prev_name,name);
						continue;
					}
				}
				if(mod != "+++") this.headerPre.set(name,i);
				if(mod != "---") this.headerPost.set(name,i);
				if(move) {
					if(this.headerMove == null) this.headerMove = new haxe.ds.StringMap();
					this.headerMove.set(name,1);
				}
			}
			if(!this.useMetaForRowChanges()) {
				if(this.source.get_height() == 0) this.applyAction("+++");
			}
		}
		,lookUp: function(del) {
			if(del == null) del = 0;
			if(this.patchInSourceRow.h.hasOwnProperty(this.currentRow + del)) return this.patchInSourceRow.h[this.currentRow + del];
			var result = -1;
			this.currentRow += del;
			if(this.currentRow >= 0 && this.currentRow < this.patch.get_height()) {
				var _g = 0;
				var _g1 = this.indexes;
				while(_g < _g1.length) {
					var idx = _g1[_g];
					++_g;
					var match = idx.queryByContent(this);
					if(match.spot_a == 0) continue;
					if(match.spot_a == 1) {
						result = match.item_a.lst[0];
						break;
					}
					if(this.currentRow > 0) {
						var prev = this.patchInSourceRow.h[this.currentRow - 1];
						if(prev != null) {
							var lst = match.item_a.lst;
							var _g2 = 0;
							while(_g2 < lst.length) {
								var row = lst[_g2];
								++_g2;
								if(row == prev + 1) {
									result = row;
									break;
								}
							}
						}
					}
				}
			}
			{
				this.patchInSourceRow.h[this.currentRow] = result;
				result;
			}
			this.currentRow -= del;
			return result;
		}
		,applyActionExternal: function(code) {
			if(code == "@@") return;
			var rc = new coopy.RowChange();
			rc.action = code;
			this.checkAct();
			if(code != "+++") rc.cond = new haxe.ds.StringMap();
			if(code != "---") rc.val = new haxe.ds.StringMap();
			var have_column = false;
			var _g1 = this.payloadCol;
			var _g = this.payloadTop;
			while(_g1 < _g) {
				var i = _g1++;
				var prev_name = this.header.h[i];
				var name = prev_name;
				if(this.headerRename.exists(prev_name)) name = this.headerRename.get(prev_name);
				var cact = this.modifier.h[i];
				if(cact == "...") continue;
				if(name == null || name == "") continue;
				var txt = this.csv.parseCell(this.getStringNull(i));
				var updated = false;
				if(this.rowInfo.updated) {
					this.getPreString(txt);
					updated = this.cellInfo.updated;
				}
				if(cact == "+++" && code != "---") {
					if(txt != null && txt != "") {
						if(rc.val == null) rc.val = new haxe.ds.StringMap();
						rc.val.set(name,txt);
						have_column = true;
					}
				}
				if(updated) {
					var value = this.csv.parseCell(this.cellInfo.lvalue);
					rc.cond.set(name,value);
					var value1 = this.csv.parseCell(this.cellInfo.rvalue);
					rc.val.set(name,value1);
				} else if(code == "+++") {
					if(cact != "---") rc.val.set(name,txt);
				} else if(cact != "+++" && cact != "---") rc.cond.set(name,txt);
			}
			if(rc.action == "+") {
				if(!have_column) return;
				rc.action = "->";
			}
			this.meta.changeRow(rc);
		}
		,applyAction: function(code) {
			if(this.useMetaForRowChanges()) {
				this.applyActionExternal(code);
				return;
			}
			var mod = new coopy.HighlightPatchUnit();
			mod.code = code;
			mod.add = code == "+++";
			mod.rem = code == "---";
			mod.update = code == "->";
			this.needSourceIndex();
			if(this.lastSourceRow == -1) this.lastSourceRow = this.lookUp(-1);
			mod.sourcePrevRow = this.lastSourceRow;
			var nextAct = this.actions[this.currentRow + 1];
			if(nextAct != "+++" && nextAct != "...") mod.sourceNextRow = this.lookUp(1);
			if(mod.add) {
				if(this.actions[this.currentRow - 1] != "+++") {
					if(this.actions[this.currentRow - 1] == "@@") {
						mod.sourcePrevRow = 0;
						this.lastSourceRow = 0;
					} else mod.sourcePrevRow = this.lookUp(-1);
				}
				mod.sourceRow = mod.sourcePrevRow;
				if(mod.sourceRow != -1) mod.sourceRowOffset = 1;
			} else mod.sourceRow = this.lastSourceRow = this.lookUp();
			if(this.actions[this.currentRow + 1] == "") this.lastSourceRow = mod.sourceNextRow;
			mod.patchRow = this.currentRow;
			if(code == "@@") mod.sourceRow = 0;
			this.mods.push(mod);
		}
		,checkAct: function() {
			var act = this.getString(this.rcOffset);
			if(this.rowInfo.value != act) coopy.DiffRender.examineCell(0,0,this.view,act,"",act,"",this.rowInfo);
		}
		,getPreString: function(txt) {
			this.checkAct();
			if(!this.rowInfo.updated) return txt;
			coopy.DiffRender.examineCell(0,0,this.view,txt,"",this.rowInfo.value,"",this.cellInfo);
			if(!this.cellInfo.updated) return txt;
			return this.cellInfo.lvalue;
		}
		,getRowString: function(c) {
			var at = this.sourceInPatchCol.h[c];
			if(at == null) return "NOT_FOUND";
			return this.getPreString(this.getString(at));
		}
		,isPreamble: function() {
			return this.currentRow <= this.preambleRow;
		}
		,sortMods: function(a,b) {
			if(b.code == "@@" && a.code != "@@") return 1;
			if(a.code == "@@" && b.code != "@@") return -1;
			if(a.sourceRow == -1 && !a.add && b.sourceRow != -1) return 1;
			if(a.sourceRow != -1 && !b.add && b.sourceRow == -1) return -1;
			if(a.sourceRow + a.sourceRowOffset > b.sourceRow + b.sourceRowOffset) return 1;
			if(a.sourceRow + a.sourceRowOffset < b.sourceRow + b.sourceRowOffset) return -1;
			if(a.patchRow > b.patchRow) return 1;
			if(a.patchRow < b.patchRow) return -1;
			return 0;
		}
		,processMods: function(rmods,fate,len) {
			rmods.sort($bind(this,this.sortMods));
			var offset = 0;
			var last = -1;
			var target = 0;
			if(rmods.length > 0) {
				if(rmods[0].sourcePrevRow == -1) last = 0;
			}
			var _g = 0;
			while(_g < rmods.length) {
				var mod = rmods[_g];
				++_g;
				if(last != -1) {
					var _g2 = last;
					var _g1 = mod.sourceRow + mod.sourceRowOffset;
					while(_g2 < _g1) {
						var i = _g2++;
						fate.push(i + offset);
						target++;
						last++;
					}
				}
				if(mod.rem) {
					fate.push(-1);
					offset--;
				} else if(mod.add) {
					mod.destRow = target;
					target++;
					offset++;
				} else mod.destRow = target;
				if(mod.sourceRow >= 0) {
					last = mod.sourceRow + mod.sourceRowOffset;
					if(mod.rem) last++;
				} else if(mod.add && mod.sourceNextRow != -1) last = mod.sourceNextRow + mod.sourceRowOffset; else if(mod.rem || mod.add) last = -1;
			}
			if(last != -1) {
				var _g3 = last;
				while(_g3 < len) {
					var i1 = _g3++;
					fate.push(i1 + offset);
					target++;
					last++;
				}
			}
			return len + offset;
		}
		,useMetaForColumnChanges: function() {
			if(this.meta == null) return false;
			return this.meta.useForColumnChanges();
		}
		,useMetaForRowChanges: function() {
			if(this.meta == null) return false;
			return this.meta.useForRowChanges();
		}
		,computeOrdering: function(mods,permutation,permutationRev,dim) {
			var to_unit_h = { };
			var from_unit_h = { };
			var meta_from_unit_h = { };
			var ct = 0;
			var _g = 0;
			while(_g < mods.length) {
				var mod = mods[_g];
				++_g;
				if(mod.add || mod.rem) continue;
				if(mod.sourceRow < 0) continue;
				if(mod.sourcePrevRow >= 0) {
					var v = mod.sourceRow;
					to_unit_h[mod.sourcePrevRow] = v;
					v;
					var v1 = mod.sourcePrevRow;
					from_unit_h[mod.sourceRow] = v1;
					v1;
					if(mod.sourcePrevRow + 1 != mod.sourceRow) ct++;
				}
				if(mod.sourceNextRow >= 0) {
					var v2 = mod.sourceNextRow;
					to_unit_h[mod.sourceRow] = v2;
					v2;
					var v3 = mod.sourceRow;
					from_unit_h[mod.sourceNextRow] = v3;
					v3;
					if(mod.sourceRow + 1 != mod.sourceNextRow) ct++;
				}
			}
			if(ct > 0) {
				var cursor = null;
				var logical = null;
				var starts = [];
				var _g1 = 0;
				while(_g1 < dim) {
					var i = _g1++;
					var u = from_unit_h[i];
					if(u != null) {
						meta_from_unit_h[u] = i;
						i;
					} else starts.push(i);
				}
				var used_h = { };
				var len = 0;
				var _g2 = 0;
				while(_g2 < dim) {
					var i1 = _g2++;
					if(logical != null && meta_from_unit_h.hasOwnProperty(logical)) cursor = meta_from_unit_h[logical]; else cursor = null;
					if(cursor == null) {
						var v4 = starts.shift();
						cursor = v4;
						logical = v4;
					}
					if(cursor == null) cursor = 0;
					while(used_h.hasOwnProperty(cursor)) cursor = (cursor + 1) % dim;
					logical = cursor;
					permutationRev.push(cursor);
					{
						used_h[cursor] = 1;
						1;
					}
				}
				var _g11 = 0;
				var _g3 = permutationRev.length;
				while(_g11 < _g3) {
					var i2 = _g11++;
					permutation[i2] = -1;
				}
				var _g12 = 0;
				var _g4 = permutation.length;
				while(_g12 < _g4) {
					var i3 = _g12++;
					permutation[permutationRev[i3]] = i3;
				}
			}
		}
		,permuteRows: function() {
			this.rowPermutation = [];
			this.rowPermutationRev = [];
			this.computeOrdering(this.mods,this.rowPermutation,this.rowPermutationRev,this.source.get_height());
		}
		,fillInNewColumns: function() {
			var _g = 0;
			var _g1 = this.cmods;
			while(_g < _g1.length) {
				var cmod = _g1[_g];
				++_g;
				if(!cmod.rem) {
					if(cmod.add) {
						var _g2 = 0;
						var _g3 = this.mods;
						while(_g2 < _g3.length) {
							var mod = _g3[_g2];
							++_g2;
							if(mod.patchRow != -1 && mod.destRow != -1) {
								var d = this.patch.getCell(cmod.patchRow,mod.patchRow);
								this.source.setCell(cmod.destRow,mod.destRow,d);
							}
						}
						var hdr = this.header.h[cmod.patchRow];
						this.source.setCell(cmod.destRow,0,this.view.toDatum(hdr));
					}
				}
			}
		}
		,finishRows: function() {
			if(this.useMetaForRowChanges()) return;
			if(this.source.get_width() == 0) {
				if(this.source.get_height() != 0) this.source.resize(0,0);
				return;
			}
			var fate = [];
			this.permuteRows();
			if(this.rowPermutation.length > 0) {
				var _g = 0;
				var _g1 = this.mods;
				while(_g < _g1.length) {
					var mod = _g1[_g];
					++_g;
					if(mod.sourceRow >= 0) mod.sourceRow = this.rowPermutation[mod.sourceRow];
				}
			}
			if(this.rowPermutation.length > 0) this.source.insertOrDeleteRows(this.rowPermutation,this.rowPermutation.length);
			var len = this.processMods(this.mods,fate,this.source.get_height());
			this.source.insertOrDeleteRows(fate,len);
			this.needDestColumns();
			var _g2 = 0;
			var _g11 = this.mods;
			while(_g2 < _g11.length) {
				var mod1 = _g11[_g2];
				++_g2;
				if(!mod1.rem) {
					if(mod1.add) {
						var $it0 = this.headerPost.iterator();
						while( $it0.hasNext() ) {
							var c = $it0.next();
							var offset = this.patchInDestCol.h[c];
							if(offset != null && offset >= 0) this.source.setCell(offset,mod1.destRow,this.patch.getCell(c,mod1.patchRow));
						}
					} else if(mod1.update) {
						this.currentRow = mod1.patchRow;
						this.checkAct();
						if(!this.rowInfo.updated) continue;
						var $it1 = this.headerPre.iterator();
						while( $it1.hasNext() ) {
							var c1 = $it1.next();
							var txt = this.view.toString(this.patch.getCell(c1,mod1.patchRow));
							coopy.DiffRender.examineCell(0,0,this.view,txt,"",this.rowInfo.value,"",this.cellInfo);
							if(!this.cellInfo.updated) continue;
							if(this.cellInfo.conflicted) continue;
							var d = this.view.toDatum(this.csv.parseCell(this.cellInfo.rvalue));
							var offset1 = this.patchInDestCol.h[c1];
							if(offset1 != null && offset1 >= 0) this.source.setCell(this.patchInDestCol.h[c1],mod1.destRow,d);
						}
					}
				}
			}
			this.fillInNewColumns();
			var _g12 = 0;
			var _g3 = this.source.get_width();
			while(_g12 < _g3) {
				var i = _g12++;
				var name = this.view.toString(this.source.getCell(i,0));
				var next_name = this.headerRename.get(name);
				if(next_name == null) continue;
				this.source.setCell(i,0,this.view.toDatum(next_name));
			}
		}
		,permuteColumns: function() {
			if(this.headerMove == null) return;
			this.colPermutation = [];
			this.colPermutationRev = [];
			this.computeOrdering(this.cmods,this.colPermutation,this.colPermutationRev,this.source.get_width());
			if(this.colPermutation.length == 0) return;
		}
		,finishColumns: function() {
			if(this.finished_columns) return;
			this.finished_columns = true;
			this.needSourceColumns();
			var _g1 = this.payloadCol;
			var _g = this.payloadTop;
			while(_g1 < _g) {
				var i = _g1++;
				var act = this.modifier.h[i];
				var hdr = this.header.h[i];
				if(act == null) act = "";
				if(act == "---") {
					var at1 = -1;
					if(this.patchInSourceCol.h.hasOwnProperty(i)) at1 = this.patchInSourceCol.h[i];
					var mod = new coopy.HighlightPatchUnit();
					mod.code = act;
					mod.rem = true;
					mod.sourceRow = at1;
					mod.patchRow = i;
					this.cmods.push(mod);
				} else if(act == "+++") {
					var mod1 = new coopy.HighlightPatchUnit();
					mod1.code = act;
					mod1.add = true;
					var prev = -1;
					var cont = false;
					mod1.sourceRow = -1;
					if(this.cmods.length > 0) mod1.sourceRow = this.cmods[this.cmods.length - 1].sourceRow;
					if(mod1.sourceRow != -1) mod1.sourceRowOffset = 1;
					mod1.patchRow = i;
					this.cmods.push(mod1);
				} else if(act != "...") {
					var at2 = -1;
					if(this.patchInSourceCol.h.hasOwnProperty(i)) at2 = this.patchInSourceCol.h[i];
					var mod2 = new coopy.HighlightPatchUnit();
					mod2.code = act;
					mod2.patchRow = i;
					mod2.sourceRow = at2;
					this.cmods.push(mod2);
				}
			}
			var at = -1;
			var rat = -1;
			var _g11 = 0;
			var _g2 = this.cmods.length - 1;
			while(_g11 < _g2) {
				var i1 = _g11++;
				var icode = this.cmods[i1].code;
				if(icode != "+++" && icode != "---") at = this.cmods[i1].sourceRow;
				this.cmods[i1 + 1].sourcePrevRow = at;
				var j = this.cmods.length - 1 - i1;
				var jcode = this.cmods[j].code;
				if(jcode != "+++" && jcode != "---") rat = this.cmods[j].sourceRow;
				this.cmods[j - 1].sourceNextRow = rat;
			}
			var fate = [];
			this.permuteColumns();
			if(this.headerMove != null) {
				if(this.colPermutation.length > 0) {
					var _g3 = 0;
					var _g12 = this.cmods;
					while(_g3 < _g12.length) {
						var mod3 = _g12[_g3];
						++_g3;
						if(mod3.sourceRow >= 0) mod3.sourceRow = this.colPermutation[mod3.sourceRow];
					}
					if(!this.useMetaForColumnChanges()) this.source.insertOrDeleteColumns(this.colPermutation,this.colPermutation.length);
				}
			}
			var len = this.processMods(this.cmods,fate,this.source.get_width());
			if(!this.useMetaForColumnChanges()) {
				this.source.insertOrDeleteColumns(fate,len);
				return;
			}
			var changed = false;
			var _g4 = 0;
			var _g13 = this.cmods;
			while(_g4 < _g13.length) {
				var mod4 = _g13[_g4];
				++_g4;
				if(mod4.code != "") {
					changed = true;
					break;
				}
			}
			if(!changed) return;
			var columns = [];
			var target_h = { };
			var inc = function(x) {
				if(x < 0) return x; else return x + 1;
			};
			var _g14 = 0;
			var _g5 = fate.length;
			while(_g14 < _g5) {
				var i2 = _g14++;
				var value = inc(fate[i2]);
				target_h[i2] = value;
			}
			this.needSourceColumns();
			this.needDestColumns();
			var _g15 = 1;
			var _g6 = this.patch.get_width();
			while(_g15 < _g6) {
				var idx_patch = _g15++;
				var change = new coopy.ColumnChange();
				var idx_src;
				if(this.patchInSourceCol.h.hasOwnProperty(idx_patch)) idx_src = this.patchInSourceCol.h[idx_patch]; else idx_src = -1;
				var prev_name = null;
				var name = null;
				if(idx_src != -1) prev_name = this.source.getCell(idx_src,0);
				if(this.modifier.h[idx_patch] != "---") {
					if(this.header.h.hasOwnProperty(idx_patch)) name = this.header.h[idx_patch];
				}
				change.prevName = prev_name;
				change.name = name;
				if(this.next_meta != null) {
					if(this.next_meta.exists(name)) change.props = this.next_meta.get(name);
				}
				columns.push(change);
			}
			this.meta.alterColumns(columns);
		}
		,__class__: coopy.HighlightPatch
	};
	coopy.HighlightPatchUnit = $hx_exports.coopy.HighlightPatchUnit = function() {
		this.add = false;
		this.rem = false;
		this.update = false;
		this.sourceRow = -1;
		this.sourceRowOffset = 0;
		this.sourcePrevRow = -1;
		this.sourceNextRow = -1;
		this.destRow = -1;
		this.patchRow = -1;
		this.code = "";
	};
	coopy.HighlightPatchUnit.__name__ = true;
	coopy.HighlightPatchUnit.prototype = {
		toString: function() {
			return "(" + this.code + " patch " + this.patchRow + " source " + this.sourcePrevRow + ":" + this.sourceRow + ":" + this.sourceNextRow + "+" + this.sourceRowOffset + " dest " + this.destRow + ")";
		}
		,__class__: coopy.HighlightPatchUnit
	};
	coopy.Index = function(flags) {
		this.items = new haxe.ds.StringMap();
		this.cols = [];
		this.keys = [];
		this.top_freq = 0;
		this.height = 0;
		this.hdr = 0;
		this.ignore_whitespace = false;
		this.ignore_case = false;
		if(flags != null) {
			this.ignore_whitespace = flags.ignore_whitespace;
			this.ignore_case = flags.ignore_case;
		}
	};
	coopy.Index.__name__ = true;
	coopy.Index.prototype = {
		addColumn: function(i) {
			this.cols.push(i);
		}
		,indexTable: function(t,hdr) {
			this.indexed_table = t;
			this.hdr = hdr;
			if(this.keys.length != t.get_height() && t.get_height() > 0) this.keys[t.get_height() - 1] = null;
			var _g1 = 0;
			var _g = t.get_height();
			while(_g1 < _g) {
				var i = _g1++;
				var key = this.keys[i];
				if(key == null) {
					key = this.toKey(t,i);
					this.keys[i] = key;
				}
				var item = this.items.get(key);
				if(item == null) {
					item = new coopy.IndexItem();
					this.items.set(key,item);
				}
				var ct;
				if(item.lst == null) item.lst = [];
				item.lst.push(i);
				ct = item.lst.length;
				if(ct > this.top_freq) this.top_freq = ct;
			}
			this.height = t.get_height();
		}
		,toKey: function(t,i) {
			var wide;
			if(i < this.hdr) wide = "_"; else wide = "";
			if(this.v == null) this.v = t.getCellView();
			var _g1 = 0;
			var _g = this.cols.length;
			while(_g1 < _g) {
				var k = _g1++;
				var d = t.getCell(this.cols[k],i);
				var txt = this.v.toString(d);
				if(this.ignore_whitespace) txt = StringTools.trim(txt);
				if(this.ignore_case) txt = txt.toLowerCase();
				if(k > 0) wide += " // ";
				if(txt == null || txt == "" || txt == "null" || txt == "undefined") continue;
				wide += txt;
			}
			return wide;
		}
		,toKeyByContent: function(row) {
			var wide;
			if(row.isPreamble()) wide = "_"; else wide = "";
			var _g1 = 0;
			var _g = this.cols.length;
			while(_g1 < _g) {
				var k = _g1++;
				var txt = row.getRowString(this.cols[k]);
				if(this.ignore_whitespace) txt = StringTools.trim(txt);
				if(this.ignore_case) txt = txt.toLowerCase();
				if(k > 0) wide += " // ";
				if(txt == null || txt == "" || txt == "null" || txt == "undefined") continue;
				wide += txt;
			}
			return wide;
		}
		,getTable: function() {
			return this.indexed_table;
		}
		,__class__: coopy.Index
	};
	coopy.IndexItem = function() {
	};
	coopy.IndexItem.__name__ = true;
	coopy.IndexItem.prototype = {
		add: function(i) {
			if(this.lst == null) this.lst = [];
			this.lst.push(i);
			return this.lst.length;
		}
		,length: function() {
			return this.lst.length;
		}
		,value: function() {
			return this.lst[0];
		}
		,asList: function() {
			return this.lst;
		}
		,__class__: coopy.IndexItem
	};
	coopy.IndexPair = function(flags) {
		this.flags = flags;
		this.ia = new coopy.Index(flags);
		this.ib = new coopy.Index(flags);
		this.quality = 0;
		this.hdr = 0;
	};
	coopy.IndexPair.__name__ = true;
	coopy.IndexPair.prototype = {
		addColumns: function(ca,cb) {
			this.ia.addColumn(ca);
			this.ib.addColumn(cb);
		}
		,indexTables: function(a,b,hdr) {
			this.ia.indexTable(a,hdr);
			this.ib.indexTable(b,hdr);
			this.hdr = hdr;
			var good = 0;
			var $it0 = this.ia.items.keys();
			while( $it0.hasNext() ) {
				var key = $it0.next();
				var item_a = this.ia.items.get(key);
				var spot_a = item_a.lst.length;
				var item_b = this.ib.items.get(key);
				var spot_b = 0;
				if(item_b != null) spot_b = item_b.lst.length;
				if(spot_a == 1 && spot_b == 1) good++;
			}
			this.quality = good / Math.max(1.0,a.get_height());
		}
		,queryByKey: function(ka) {
			var result = new coopy.CrossMatch();
			result.item_a = this.ia.items.get(ka);
			result.item_b = this.ib.items.get(ka);
			result.spot_a = result.spot_b = 0;
			if(ka != "") {
				if(result.item_a != null) result.spot_a = result.item_a.lst.length;
				if(result.item_b != null) result.spot_b = result.item_b.lst.length;
			}
			return result;
		}
		,queryByContent: function(row) {
			var result = new coopy.CrossMatch();
			var ka = this.ia.toKeyByContent(row);
			return this.queryByKey(ka);
		}
		,queryLocal: function(row) {
			var ka = this.ia.toKey(this.ia.getTable(),row);
			return this.queryByKey(ka);
		}
		,localKey: function(row) {
			return this.ia.toKey(this.ia.getTable(),row);
		}
		,remoteKey: function(row) {
			return this.ib.toKey(this.ib.getTable(),row);
		}
		,getTopFreq: function() {
			if(this.ib.top_freq > this.ia.top_freq) return this.ib.top_freq;
			return this.ia.top_freq;
		}
		,getQuality: function() {
			return this.quality;
		}
		,__class__: coopy.IndexPair
	};
	coopy.Meta = function() { };
	coopy.Meta.__name__ = true;
	coopy.Meta.prototype = {
		__class__: coopy.Meta
	};
	coopy.JsonTable = function(data,name) {
		this.data = data;
		this.columns = Reflect.field(data,"columns");
		this.rows = Reflect.field(data,"rows");
		this.w = this.columns.length;
		this.h = this.rows.length;
		this.idx2col = new haxe.ds.IntMap();
		var _g1 = 0;
		var _g = this.columns.length;
		while(_g1 < _g) {
			var idx = _g1++;
			var v = this.columns[idx];
			this.idx2col.h[idx] = v;
			v;
		}
		this.name = name;
	};
	coopy.JsonTable.__name__ = true;
	coopy.JsonTable.__interfaces__ = [coopy.Meta,coopy.Table];
	coopy.JsonTable.prototype = {
		getTable: function() {
			return this;
		}
		,get_width: function() {
			return this.w;
		}
		,get_height: function() {
			return this.h + 1;
		}
		,getCell: function(x,y) {
			if(y == 0) return this.idx2col.h[x];
			return Reflect.field(this.rows[y - 1],this.idx2col.h[x]);
		}
		,setCell: function(x,y,c) {
			console.log("JsonTable is read-only");
		}
		,toString: function() {
			return "";
		}
		,getCellView: function() {
			return new coopy.SimpleView();
		}
		,isResizable: function() {
			return false;
		}
		,resize: function(w,h) {
			return false;
		}
		,clear: function() {
		}
		,insertOrDeleteRows: function(fate,hfate) {
			return false;
		}
		,insertOrDeleteColumns: function(fate,wfate) {
			return false;
		}
		,trimBlank: function() {
			return false;
		}
		,getData: function() {
			return null;
		}
		,clone: function() {
			return null;
		}
		,setMeta: function(meta) {
		}
		,getMeta: function() {
			return this;
		}
		,create: function() {
			return null;
		}
		,alterColumns: function(columns) {
			return false;
		}
		,changeRow: function(rc) {
			return false;
		}
		,applyFlags: function(flags) {
			return false;
		}
		,asTable: function() {
			return null;
		}
		,cloneMeta: function(table) {
			return null;
		}
		,useForColumnChanges: function() {
			return false;
		}
		,useForRowChanges: function() {
			return false;
		}
		,getRowStream: function() {
			return null;
		}
		,isNested: function() {
			return false;
		}
		,isSql: function() {
			return false;
		}
		,getName: function() {
			return this.name;
		}
		,__class__: coopy.JsonTable
	};
	coopy.JsonTables = function(json,flags) {
		this.db = json;
		var names = Reflect.field(json,"names");
		var allowed = null;
		var count = names.length;
		if(flags != null && flags.tables != null) {
			allowed = new haxe.ds.StringMap();
			var _g = 0;
			var _g1 = flags.tables;
			while(_g < _g1.length) {
				var name = _g1[_g];
				++_g;
				if(__map_reserved[name] != null) allowed.setReserved(name,true); else allowed.h[name] = true;
			}
			count = 0;
			var _g2 = 0;
			while(_g2 < names.length) {
				var name1 = names[_g2];
				++_g2;
				if(__map_reserved[name1] != null?allowed.existsReserved(name1):allowed.h.hasOwnProperty(name1)) count++;
			}
		}
		this.t = new coopy.SimpleTable(2,count + 1);
		this.t.setCell(0,0,"name");
		this.t.setCell(1,0,"table");
		var v = this.t.getCellView();
		var at = 1;
		var _g3 = 0;
		while(_g3 < names.length) {
			var name2 = names[_g3];
			++_g3;
			if(allowed != null) {
				if(!(__map_reserved[name2] != null?allowed.existsReserved(name2):allowed.h.hasOwnProperty(name2))) continue;
			}
			this.t.setCell(0,at,name2);
			var tab = Reflect.field(this.db,"tables");
			tab = Reflect.field(tab,name2);
			this.t.setCell(1,at,v.wrapTable(new coopy.JsonTable(tab,name2)));
			at++;
		}
	};
	coopy.JsonTables.__name__ = true;
	coopy.JsonTables.__interfaces__ = [coopy.Table];
	coopy.JsonTables.prototype = {
		getCell: function(x,y) {
			return this.t.getCell(x,y);
		}
		,setCell: function(x,y,c) {
		}
		,getCellView: function() {
			return this.t.getCellView();
		}
		,isResizable: function() {
			return false;
		}
		,resize: function(w,h) {
			return false;
		}
		,clear: function() {
		}
		,insertOrDeleteRows: function(fate,hfate) {
			return false;
		}
		,insertOrDeleteColumns: function(fate,wfate) {
			return false;
		}
		,trimBlank: function() {
			return false;
		}
		,get_width: function() {
			return this.t.get_width();
		}
		,get_height: function() {
			return this.t.get_height();
		}
		,getData: function() {
			return null;
		}
		,clone: function() {
			return null;
		}
		,getMeta: function() {
			return new coopy.SimpleMeta(this,true,true);
		}
		,create: function() {
			return null;
		}
		,__class__: coopy.JsonTables
	};
	coopy.Merger = $hx_exports.coopy.Merger = function(parent,local,remote,flags) {
		this.parent = parent;
		this.local = local;
		this.remote = remote;
		this.flags = flags;
	};
	coopy.Merger.__name__ = true;
	coopy.Merger.makeConflictedCell = function(view,pcell,lcell,rcell) {
		return view.toDatum("((( " + view.toString(pcell) + " ))) " + view.toString(lcell) + " /// " + view.toString(rcell));
	};
	coopy.Merger.prototype = {
		shuffleDimension: function(dim_units,len,fate,cl,cr) {
			var at = 0;
			var _g = 0;
			while(_g < dim_units.length) {
				var cunit = dim_units[_g];
				++_g;
				if(cunit.p < 0) {
					if(cunit.l < 0) {
						if(cunit.r >= 0) {
							{
								cr.h[cunit.r] = at;
								at;
							}
							at++;
						}
					} else {
						{
							cl.h[cunit.l] = at;
							at;
						}
						at++;
					}
				} else if(cunit.l >= 0) {
					if(cunit.r < 0) {
					} else {
						{
							cl.h[cunit.l] = at;
							at;
						}
						at++;
					}
				}
			}
			var _g1 = 0;
			while(_g1 < len) {
				var x = _g1++;
				var idx = cl.h[x];
				if(idx == null) fate.push(-1); else fate.push(idx);
			}
			return at;
		}
		,shuffleColumns: function() {
			this.column_mix_local = new haxe.ds.IntMap();
			this.column_mix_remote = new haxe.ds.IntMap();
			var fate = [];
			var wfate = this.shuffleDimension(this.column_units,this.local.get_width(),fate,this.column_mix_local,this.column_mix_remote);
			this.local.insertOrDeleteColumns(fate,wfate);
		}
		,shuffleRows: function() {
			this.row_mix_local = new haxe.ds.IntMap();
			this.row_mix_remote = new haxe.ds.IntMap();
			var fate = [];
			var hfate = this.shuffleDimension(this.units,this.local.get_height(),fate,this.row_mix_local,this.row_mix_remote);
			this.local.insertOrDeleteRows(fate,hfate);
		}
		,apply: function() {
			this.conflicts = 0;
			var ct = coopy.Coopy.compareTables3(this.parent,this.local,this.remote);
			var align = ct.align();
			this.order = align.toOrder();
			this.units = this.order.getList();
			this.column_order = align.meta.toOrder();
			this.column_units = this.column_order.getList();
			var allow_insert = this.flags.allowInsert();
			var allow_delete = this.flags.allowDelete();
			var allow_update = this.flags.allowUpdate();
			var view = this.parent.getCellView();
			var _g = 0;
			var _g1 = this.units;
			while(_g < _g1.length) {
				var row = _g1[_g];
				++_g;
				if(row.l >= 0 && row.r >= 0 && row.p >= 0) {
					var _g2 = 0;
					var _g3 = this.column_units;
					while(_g2 < _g3.length) {
						var col = _g3[_g2];
						++_g2;
						if(col.l >= 0 && col.r >= 0 && col.p >= 0) {
							var pcell = this.parent.getCell(col.p,row.p);
							var rcell = this.remote.getCell(col.r,row.r);
							if(!view.equals(pcell,rcell)) {
								var lcell = this.local.getCell(col.l,row.l);
								if(view.equals(pcell,lcell)) this.local.setCell(col.l,row.l,rcell); else {
									this.local.setCell(col.l,row.l,coopy.Merger.makeConflictedCell(view,pcell,lcell,rcell));
									this.conflicts++;
								}
							}
						}
					}
				}
			}
			this.shuffleColumns();
			this.shuffleRows();
			var $it0 = this.column_mix_remote.keys();
			while( $it0.hasNext() ) {
				var x = $it0.next();
				var x2 = this.column_mix_remote.h[x];
				var _g4 = 0;
				var _g11 = this.units;
				while(_g4 < _g11.length) {
					var unit = _g11[_g4];
					++_g4;
					if(unit.l >= 0 && unit.r >= 0) this.local.setCell(x2,this.row_mix_local.h[unit.l],this.remote.getCell(x,unit.r)); else if(unit.p < 0 && unit.r >= 0) this.local.setCell(x2,this.row_mix_remote.h[unit.r],this.remote.getCell(x,unit.r));
				}
			}
			var $it1 = this.row_mix_remote.keys();
			while( $it1.hasNext() ) {
				var y = $it1.next();
				var y2 = this.row_mix_remote.h[y];
				var _g5 = 0;
				var _g12 = this.column_units;
				while(_g5 < _g12.length) {
					var unit1 = _g12[_g5];
					++_g5;
					if(unit1.l >= 0 && unit1.r >= 0) this.local.setCell(this.column_mix_local.h[unit1.l],y2,this.remote.getCell(unit1.r,y));
				}
			}
			return this.conflicts;
		}
		,__class__: coopy.Merger
	};
	coopy.Mover = $hx_exports.coopy.Mover = function() { };
	coopy.Mover.__name__ = true;
	coopy.Mover.moveUnits = function(units) {
		var isrc = [];
		var idest = [];
		var len = units.length;
		var ltop = -1;
		var rtop = -1;
		var in_src_h = { };
		var in_dest_h = { };
		var _g = 0;
		while(_g < len) {
			var i = _g++;
			var unit = units[i];
			if(unit.l >= 0 && unit.r >= 0) {
				if(ltop < unit.l) ltop = unit.l;
				if(rtop < unit.r) rtop = unit.r;
				{
					in_src_h[unit.l] = i;
					i;
				}
				{
					in_dest_h[unit.r] = i;
					i;
				}
			}
		}
		var v;
		var _g1 = 0;
		var _g2 = ltop + 1;
		while(_g1 < _g2) {
			var i1 = _g1++;
			v = in_src_h[i1];
			if(v != null) isrc.push(v);
		}
		var _g11 = 0;
		var _g3 = rtop + 1;
		while(_g11 < _g3) {
			var i2 = _g11++;
			v = in_dest_h[i2];
			if(v != null) idest.push(v);
		}
		return coopy.Mover.moveWithoutExtras(isrc,idest);
	};
	coopy.Mover.move = function(isrc,idest) {
		var len = isrc.length;
		var len2 = idest.length;
		var in_src_h = { };
		var in_dest_h = { };
		var _g = 0;
		while(_g < len) {
			var i = _g++;
			{
				in_src_h[isrc[i]] = i;
				i;
			}
		}
		var _g1 = 0;
		while(_g1 < len2) {
			var i1 = _g1++;
			{
				in_dest_h[idest[i1]] = i1;
				i1;
			}
		}
		var src = [];
		var dest = [];
		var v;
		var _g2 = 0;
		while(_g2 < len) {
			var i2 = _g2++;
			v = isrc[i2];
			if(in_dest_h.hasOwnProperty(v)) src.push(v);
		}
		var _g3 = 0;
		while(_g3 < len2) {
			var i3 = _g3++;
			v = idest[i3];
			if(in_src_h.hasOwnProperty(v)) dest.push(v);
		}
		return coopy.Mover.moveWithoutExtras(src,dest);
	};
	coopy.Mover.moveWithoutExtras = function(src,dest) {
		if(src.length != dest.length) return null;
		if(src.length <= 1) return [];
		var len = src.length;
		var in_src_h = { };
		var blk_len = new haxe.ds.IntMap();
		var blk_src_loc_h = { };
		var blk_dest_loc_h = { };
		var _g = 0;
		while(_g < len) {
			var i = _g++;
			{
				in_src_h[src[i]] = i;
				i;
			}
		}
		var ct = 0;
		var in_cursor = -2;
		var out_cursor = 0;
		var next;
		var blk = -1;
		var v;
		while(out_cursor < len) {
			v = dest[out_cursor];
			next = in_src_h[v];
			if(next != in_cursor + 1) {
				blk = v;
				ct = 1;
				blk_src_loc_h[blk] = next;
				blk_dest_loc_h[blk] = out_cursor;
			} else ct++;
			blk_len.h[blk] = ct;
			in_cursor = next;
			out_cursor++;
		}
		var blks = [];
		var $it0 = blk_len.keys();
		while( $it0.hasNext() ) {
			var k = $it0.next();
			blks.push(k);
		}
		blks.sort(function(a,b) {
			var diff = blk_len.h[b] - blk_len.h[a];
			if(diff != 0) return diff;
			return a - b;
		});
		var moved = [];
		while(blks.length > 0) {
			var blk1 = blks.shift();
			var blen = blks.length;
			var ref_src_loc = blk_src_loc_h[blk1];
			var ref_dest_loc = blk_dest_loc_h[blk1];
			var i1 = blen - 1;
			while(i1 >= 0) {
				var blki = blks[i1];
				var blki_src_loc = blk_src_loc_h[blki];
				var to_left_src = blki_src_loc < ref_src_loc;
				var to_left_dest = blk_dest_loc_h[blki] < ref_dest_loc;
				if(to_left_src != to_left_dest) {
					var ct1 = blk_len.h[blki];
					var _g1 = 0;
					while(_g1 < ct1) {
						var j = _g1++;
						moved.push(src[blki_src_loc]);
						blki_src_loc++;
					}
					blks.splice(i1,1);
				}
				i1--;
			}
		}
		return moved;
	};
	coopy.Ndjson = $hx_exports.coopy.Ndjson = function(tab) {
		this.tab = tab;
		this.view = tab.getCellView();
		this.header_row = 0;
	};
	coopy.Ndjson.__name__ = true;
	coopy.Ndjson.prototype = {
		renderRow: function(r) {
			var row = new haxe.ds.StringMap();
			var _g1 = 0;
			var _g = this.tab.get_width();
			while(_g1 < _g) {
				var c = _g1++;
				var key = this.view.toString(this.tab.getCell(c,this.header_row));
				if(c == 0 && this.header_row == 1) key = "@:@";
				var value = this.tab.getCell(c,r);
				row.set(key,value);
			}
			return haxe.format.JsonPrinter.print(row,null,null);
		}
		,render: function() {
			var txt = "";
			var offset = 0;
			if(this.tab.get_height() == 0) return txt;
			if(this.tab.get_width() == 0) return txt;
			if(this.tab.getCell(0,0) == "@:@") offset = 1;
			this.header_row = offset;
			var _g1 = this.header_row + 1;
			var _g = this.tab.get_height();
			while(_g1 < _g) {
				var r = _g1++;
				txt += this.renderRow(r);
				txt += "\n";
			}
			return txt;
		}
		,addRow: function(r,txt) {
			var json = new haxe.format.JsonParser(txt).parseRec();
			if(this.columns == null) this.columns = new haxe.ds.StringMap();
			var w = this.tab.get_width();
			var h = this.tab.get_height();
			var resize = false;
			var _g = 0;
			var _g1 = Reflect.fields(json);
			while(_g < _g1.length) {
				var name = _g1[_g];
				++_g;
				if(!this.columns.exists(name)) {
					this.columns.set(name,w);
					w++;
					resize = true;
				}
			}
			if(r >= h) {
				h = r + 1;
				resize = true;
			}
			if(resize) this.tab.resize(w,h);
			var _g2 = 0;
			var _g11 = Reflect.fields(json);
			while(_g2 < _g11.length) {
				var name1 = _g11[_g2];
				++_g2;
				var v = Reflect.field(json,name1);
				var c = this.columns.get(name1);
				this.tab.setCell(c,r,v);
			}
		}
		,addHeaderRow: function(r) {
			var names = this.columns.keys();
			while( names.hasNext() ) {
				var n = names.next();
				this.tab.setCell(this.columns.get(n),r,this.view.toDatum(n));
			}
		}
		,parse: function(txt) {
			this.columns = null;
			var rows = txt.split("\n");
			var h = rows.length;
			if(h == 0) {
				this.tab.clear();
				return;
			}
			if(rows[h - 1] == "") h--;
			var _g = 0;
			while(_g < h) {
				var i = _g++;
				var at = h - i - 1;
				this.addRow(at + 1,rows[at]);
			}
			this.addHeaderRow(0);
		}
		,__class__: coopy.Ndjson
	};
	coopy.NestedCellBuilder = function() {
	};
	coopy.NestedCellBuilder.__name__ = true;
	coopy.NestedCellBuilder.__interfaces__ = [coopy.CellBuilder];
	coopy.NestedCellBuilder.prototype = {
		needSeparator: function() {
			return false;
		}
		,setSeparator: function(separator) {
		}
		,setConflictSeparator: function(separator) {
		}
		,setView: function(view) {
			this.view = view;
		}
		,update: function(local,remote) {
			var h = this.view.makeHash();
			this.view.hashSet(h,"before",local);
			this.view.hashSet(h,"after",remote);
			return h;
		}
		,conflict: function(parent,local,remote) {
			var h = this.view.makeHash();
			this.view.hashSet(h,"before",parent);
			this.view.hashSet(h,"ours",local);
			this.view.hashSet(h,"theirs",remote);
			return h;
		}
		,marker: function(label) {
			return this.view.toDatum(label);
		}
		,negToNull: function(x) {
			if(x < 0) return null;
			return x;
		}
		,links: function(unit,row_like) {
			var h = this.view.makeHash();
			if(unit.p >= -1) {
				this.view.hashSet(h,"before",this.negToNull(unit.p));
				this.view.hashSet(h,"ours",this.negToNull(unit.l));
				this.view.hashSet(h,"theirs",this.negToNull(unit.r));
				return h;
			}
			this.view.hashSet(h,"before",this.negToNull(unit.l));
			this.view.hashSet(h,"after",this.negToNull(unit.r));
			return h;
		}
		,__class__: coopy.NestedCellBuilder
	};
	coopy.Ordering = function() {
		this.order = [];
		this.ignore_parent = false;
	};
	coopy.Ordering.__name__ = true;
	coopy.Ordering.prototype = {
		add: function(l,r,p) {
			if(p == null) p = -2;
			if(this.ignore_parent) p = -2;
			this.order.push(new coopy.Unit(l,r,p));
		}
		,getList: function() {
			return this.order;
		}
		,setList: function(lst) {
			this.order = lst;
		}
		,toString: function() {
			var txt = "";
			var _g1 = 0;
			var _g = this.order.length;
			while(_g1 < _g) {
				var i = _g1++;
				if(i > 0) txt += ", ";
				txt += Std.string(this.order[i]);
			}
			return txt;
		}
		,ignoreParent: function() {
			this.ignore_parent = true;
		}
		,__class__: coopy.Ordering
	};
	coopy.PropertyChange = function() {
	};
	coopy.PropertyChange.__name__ = true;
	coopy.PropertyChange.prototype = {
		__class__: coopy.PropertyChange
	};
	coopy.RowChange = $hx_exports.coopy.RowChange = function() {
	};
	coopy.RowChange.__name__ = true;
	coopy.RowChange.prototype = {
		showMap: function(m) {
			if(m == null) return "{}";
			var txt = "";
			var $it0 = m.keys();
			while( $it0.hasNext() ) {
				var k = $it0.next();
				if(txt != "") txt += ", ";
				var v;
				v = __map_reserved[k] != null?m.getReserved(k):m.h[k];
				txt += k + "=" + Std.string(v);
			}
			return "{ " + txt + " }";
		}
		,toString: function() {
			return this.action + " " + this.showMap(this.cond) + " : " + this.showMap(this.val);
		}
		,__class__: coopy.RowChange
	};
	coopy.RowStream = function() { };
	coopy.RowStream.__name__ = true;
	coopy.RowStream.prototype = {
		__class__: coopy.RowStream
	};
	coopy.SimpleMeta = $hx_exports.coopy.SimpleMeta = function(t,has_properties,may_be_nested) {
		if(may_be_nested == null) may_be_nested = false;
		if(has_properties == null) has_properties = true;
		this.t = t;
		this.rowChange();
		this.colChange();
		this.has_properties = has_properties;
		this.may_be_nested = may_be_nested;
		this.metadata = null;
		this.keys = null;
		this.row_active = false;
		this.row_change_cache = null;
	};
	coopy.SimpleMeta.__name__ = true;
	coopy.SimpleMeta.__interfaces__ = [coopy.Meta];
	coopy.SimpleMeta.prototype = {
		storeRowChanges: function(changes) {
			this.row_change_cache = changes;
			this.row_active = true;
		}
		,rowChange: function() {
			this.name2row = null;
		}
		,colChange: function() {
			this.name2col = null;
		}
		,col: function(key) {
			if(this.t.get_height() < 1) return -1;
			if(this.name2col == null) {
				this.name2col = new haxe.ds.StringMap();
				var w = this.t.get_width();
				var _g = 0;
				while(_g < w) {
					var c = _g++;
					var key1 = this.t.getCell(c,0);
					this.name2col.set(key1,c);
				}
			}
			if(!this.name2col.exists(key)) return -1;
			return this.name2col.get(key);
		}
		,row: function(key) {
			if(this.t.get_width() < 1) return -1;
			if(this.name2row == null) {
				this.name2row = new haxe.ds.StringMap();
				var h = this.t.get_height();
				var _g = 1;
				while(_g < h) {
					var r = _g++;
					var key1 = this.t.getCell(0,r);
					this.name2row.set(key1,r);
				}
			}
			if(!this.name2row.exists(key)) return -1;
			return this.name2row.get(key);
		}
		,alterColumns: function(columns) {
			var target = new haxe.ds.StringMap();
			var wfate = 0;
			if(this.has_properties) {
				if(__map_reserved["@"] != null) target.setReserved("@",wfate); else target.h["@"] = wfate;
				wfate++;
			}
			var _g1 = 0;
			var _g = columns.length;
			while(_g1 < _g) {
				var i = _g1++;
				var col = columns[i];
				if(col.prevName != null) target.set(col.prevName,wfate);
				if(col.name != null) wfate++;
			}
			var fate = [];
			var _g11 = 0;
			var _g2 = this.t.get_width();
			while(_g11 < _g2) {
				var i1 = _g11++;
				var targeti = -1;
				var name = this.t.getCell(i1,0);
				if(__map_reserved[name] != null?target.existsReserved(name):target.h.hasOwnProperty(name)) targeti = __map_reserved[name] != null?target.getReserved(name):target.h[name];
				fate.push(targeti);
			}
			this.t.insertOrDeleteColumns(fate,wfate);
			var start;
			if(this.has_properties) start = 1; else start = 0;
			var at = start;
			var _g12 = 0;
			var _g3 = columns.length;
			while(_g12 < _g3) {
				var i2 = _g12++;
				var col1 = columns[i2];
				if(col1.name != null) {
					if(col1.name != col1.prevName) this.t.setCell(at,0,col1.name);
				}
				if(col1.name != null) at++;
			}
			if(!this.has_properties) return true;
			this.colChange();
			at = start;
			var _g13 = 0;
			var _g4 = columns.length;
			while(_g13 < _g4) {
				var i3 = _g13++;
				var col2 = columns[i3];
				if(col2.name != null) {
					var _g21 = 0;
					var _g31 = col2.props;
					while(_g21 < _g31.length) {
						var prop = _g31[_g21];
						++_g21;
						this.setCell(col2.name,prop.name,prop.val);
					}
				}
				if(col2.name != null) at++;
			}
			return true;
		}
		,setCell: function(c,r,val) {
			var ri = this.row(r);
			if(ri == -1) return false;
			var ci = this.col(c);
			if(ci == -1) return false;
			this.t.setCell(ci,ri,val);
			return true;
		}
		,addMetaData: function(column,property,val) {
			if(this.metadata == null) {
				this.metadata = new haxe.ds.StringMap();
				this.keys = new haxe.ds.StringMap();
			}
			if(!this.metadata.exists(column)) {
				var value = new haxe.ds.StringMap();
				this.metadata.set(column,value);
			}
			var props = this.metadata.get(column);
			var value1 = val;
			props.set(property,value1);
			this.keys.set(property,true);
		}
		,asTable: function() {
			if(this.has_properties && this.metadata == null) return this.t;
			if(this.metadata == null) return null;
			var w = this.t.get_width();
			var props = [];
			var $it0 = this.keys.keys();
			while( $it0.hasNext() ) {
				var k = $it0.next();
				props.push(k);
			}
			props.sort(Reflect.compare);
			var mt = new coopy.SimpleTable(w + 1,props.length + 1);
			mt.setCell(0,0,"@");
			var _g = 0;
			while(_g < w) {
				var x = _g++;
				var name = this.t.getCell(x,0);
				mt.setCell(1 + x,0,name);
				if(!this.metadata.exists(name)) continue;
				var vals = this.metadata.get(name);
				var _g2 = 0;
				var _g1 = props.length;
				while(_g2 < _g1) {
					var i = _g2++;
					if(vals.exists(props[i])) mt.setCell(1 + x,i + 1,vals.get(props[i]));
				}
			}
			var _g11 = 0;
			var _g3 = props.length;
			while(_g11 < _g3) {
				var y = _g11++;
				mt.setCell(0,y + 1,props[y]);
			}
			return mt;
		}
		,cloneMeta: function(table) {
			var result = new coopy.SimpleMeta(table);
			if(this.metadata != null) {
				result.keys = new haxe.ds.StringMap();
				var $it0 = this.keys.keys();
				while( $it0.hasNext() ) {
					var k = $it0.next();
					result.keys.set(k,true);
				}
				result.metadata = new haxe.ds.StringMap();
				var $it1 = this.metadata.keys();
				while( $it1.hasNext() ) {
					var k1 = $it1.next();
					if(!this.metadata.exists(k1)) continue;
					var vals = this.metadata.get(k1);
					var nvals = new haxe.ds.StringMap();
					var $it2 = vals.keys();
					while( $it2.hasNext() ) {
						var p = $it2.next();
						var value;
						value = __map_reserved[p] != null?vals.getReserved(p):vals.h[p];
						nvals.set(p,value);
					}
					result.metadata.set(k1,nvals);
				}
			}
			return result;
		}
		,useForColumnChanges: function() {
			return true;
		}
		,useForRowChanges: function() {
			return this.row_active;
		}
		,changeRow: function(rc) {
			this.row_change_cache.push(rc);
			return false;
		}
		,applyFlags: function(flags) {
			return false;
		}
		,getRowStream: function() {
			return new coopy.TableStream(this.t);
		}
		,isNested: function() {
			return this.may_be_nested;
		}
		,isSql: function() {
			return false;
		}
		,getName: function() {
			return null;
		}
		,__class__: coopy.SimpleMeta
	};
	coopy.SimpleTable = $hx_exports.coopy.SimpleTable = function(w,h) {
		this.data = new haxe.ds.IntMap();
		this.w = w;
		this.h = h;
		this.meta = null;
	};
	coopy.SimpleTable.__name__ = true;
	coopy.SimpleTable.__interfaces__ = [coopy.Table];
	coopy.SimpleTable.tableToString = function(tab) {
		var meta = tab.getMeta();
		if(meta != null) {
			var stream = meta.getRowStream();
			if(stream != null) {
				var x1 = "";
				var cols = stream.fetchColumns();
				var _g1 = 0;
				var _g = cols.length;
				while(_g1 < _g) {
					var i = _g1++;
					if(i > 0) x1 += ",";
					x1 += cols[i];
				}
				x1 += "\n";
				var row = stream.fetchRow();
				while(row != null) {
					var _g11 = 0;
					var _g2 = cols.length;
					while(_g11 < _g2) {
						var i1 = _g11++;
						if(i1 > 0) x1 += ",";
						x1 += Std.string(row.get(cols[i1]));
					}
					x1 += "\n";
					row = stream.fetchRow();
				}
				return x1;
			}
		}
		var x = "";
		var _g12 = 0;
		var _g3 = tab.get_height();
		while(_g12 < _g3) {
			var i2 = _g12++;
			var _g31 = 0;
			var _g21 = tab.get_width();
			while(_g31 < _g21) {
				var j = _g31++;
				if(j > 0) x += ",";
				x += Std.string(tab.getCell(j,i2));
			}
			x += "\n";
		}
		return x;
	};
	coopy.SimpleTable.tableIsSimilar = function(tab1,tab2) {
		if(tab1.get_height() == -1 || tab2.get_height() == -1) {
			var txt1 = coopy.SimpleTable.tableToString(tab1);
			var txt2 = coopy.SimpleTable.tableToString(tab2);
			return txt1 == txt2;
		}
		if(tab1.get_width() != tab2.get_width()) return false;
		if(tab1.get_height() != tab2.get_height()) return false;
		var v = tab1.getCellView();
		var _g1 = 0;
		var _g = tab1.get_height();
		while(_g1 < _g) {
			var i = _g1++;
			var _g3 = 0;
			var _g2 = tab1.get_width();
			while(_g3 < _g2) {
				var j = _g3++;
				if(!v.equals(tab1.getCell(j,i),tab2.getCell(j,i))) return false;
			}
		}
		return true;
	};
	coopy.SimpleTable.prototype = {
		getTable: function() {
			return this;
		}
		,get_width: function() {
			return this.w;
		}
		,get_height: function() {
			return this.h;
		}
		,getCell: function(x,y) {
			return this.data.h[x + y * this.w];
		}
		,setCell: function(x,y,c) {
			var value = c;
			this.data.set(x + y * this.w,value);
		}
		,toString: function() {
			return coopy.SimpleTable.tableToString(this);
		}
		,getCellView: function() {
			return new coopy.SimpleView();
		}
		,isResizable: function() {
			return true;
		}
		,resize: function(w,h) {
			this.w = w;
			this.h = h;
			return true;
		}
		,clear: function() {
			this.data = new haxe.ds.IntMap();
		}
		,insertOrDeleteRows: function(fate,hfate) {
			var data2 = new haxe.ds.IntMap();
			var _g1 = 0;
			var _g = fate.length;
			while(_g1 < _g) {
				var i = _g1++;
				var j = fate[i];
				if(j != -1) {
					var _g3 = 0;
					var _g2 = this.w;
					while(_g3 < _g2) {
						var c = _g3++;
						var idx = i * this.w + c;
						if(this.data.h.hasOwnProperty(idx)) {
							var value = this.data.h[idx];
							data2.set(j * this.w + c,value);
						}
					}
				}
			}
			this.h = hfate;
			this.data = data2;
			return true;
		}
		,insertOrDeleteColumns: function(fate,wfate) {
			var data2 = new haxe.ds.IntMap();
			var _g1 = 0;
			var _g = fate.length;
			while(_g1 < _g) {
				var i = _g1++;
				var j = fate[i];
				if(j != -1) {
					var _g3 = 0;
					var _g2 = this.h;
					while(_g3 < _g2) {
						var r = _g3++;
						var idx = r * this.w + i;
						if(this.data.h.hasOwnProperty(idx)) {
							var value = this.data.h[idx];
							data2.set(r * wfate + j,value);
						}
					}
				}
			}
			this.w = wfate;
			this.data = data2;
			return true;
		}
		,trimBlank: function() {
			if(this.h == 0) return true;
			var h_test = this.h;
			if(h_test >= 3) h_test = 3;
			var view = this.getCellView();
			var space = view.toDatum("");
			var more = true;
			while(more) {
				var _g1 = 0;
				var _g = this.get_width();
				while(_g1 < _g) {
					var i = _g1++;
					var c = this.getCell(i,this.h - 1);
					if(!(view.equals(c,space) || c == null)) {
						more = false;
						break;
					}
				}
				if(more) this.h--;
			}
			more = true;
			var nw = this.w;
			while(more) {
				if(this.w == 0) break;
				var _g2 = 0;
				while(_g2 < h_test) {
					var i1 = _g2++;
					var c1 = this.getCell(nw - 1,i1);
					if(!(view.equals(c1,space) || c1 == null)) {
						more = false;
						break;
					}
				}
				if(more) nw--;
			}
			if(nw == this.w) return true;
			var data2 = new haxe.ds.IntMap();
			var _g3 = 0;
			while(_g3 < nw) {
				var i2 = _g3++;
				var _g21 = 0;
				var _g11 = this.h;
				while(_g21 < _g11) {
					var r = _g21++;
					var idx = r * this.w + i2;
					if(this.data.h.hasOwnProperty(idx)) {
						var value = this.data.h[idx];
						data2.set(r * nw + i2,value);
					}
				}
			}
			this.w = nw;
			this.data = data2;
			return true;
		}
		,getData: function() {
			return null;
		}
		,clone: function() {
			var result = new coopy.SimpleTable(this.get_width(),this.get_height());
			var _g1 = 0;
			var _g = this.get_height();
			while(_g1 < _g) {
				var i = _g1++;
				var _g3 = 0;
				var _g2 = this.get_width();
				while(_g3 < _g2) {
					var j = _g3++;
					result.setCell(j,i,this.getCell(j,i));
				}
			}
			if(this.meta != null) result.meta = this.meta.cloneMeta(result);
			return result;
		}
		,create: function() {
			return new coopy.SimpleTable(this.get_width(),this.get_height());
		}
		,setMeta: function(meta) {
			this.meta = meta;
		}
		,getMeta: function() {
			return this.meta;
		}
		,__class__: coopy.SimpleTable
	};
	coopy.View = function() { };
	coopy.View.__name__ = true;
	coopy.View.prototype = {
		__class__: coopy.View
	};
	coopy.SimpleView = $hx_exports.coopy.SimpleView = function() {
	};
	coopy.SimpleView.__name__ = true;
	coopy.SimpleView.__interfaces__ = [coopy.View];
	coopy.SimpleView.prototype = {
		toString: function(d) {
			if(d == null) return "";
			return "" + Std.string(d);
		}
		,equals: function(d1,d2) {
			if(d1 == null && d2 == null) return true;
			if(d1 == null && "" + Std.string(d2) == "") return true;
			if("" + Std.string(d1) == "" && d2 == null) return true;
			return "" + Std.string(d1) == "" + Std.string(d2);
		}
		,toDatum: function(x) {
			return x;
		}
		,makeHash: function() {
			return new haxe.ds.StringMap();
		}
		,hashSet: function(h,str,d) {
			var hh = h;
			var value = d;
			hh.set(str,value);
		}
		,hashExists: function(h,str) {
			var hh = h;
			if(__map_reserved[str] != null) return hh.existsReserved(str); else return hh.h.hasOwnProperty(str);
		}
		,hashGet: function(h,str) {
			var hh = h;
			return __map_reserved[str] != null?hh.getReserved(str):hh.h[str];
		}
		,isHash: function(h) {
			return js.Boot.__instanceof(h,haxe.ds.StringMap);
		}
		,isTable: function(t) {
			return js.Boot.__instanceof(t,coopy.Table);
		}
		,getTable: function(t) {
			return t;
		}
		,wrapTable: function(t) {
			return t;
		}
		,__class__: coopy.SimpleView
	};
	coopy.SparseSheet = function() {
		this.h = this.w = 0;
	};
	coopy.SparseSheet.__name__ = true;
	coopy.SparseSheet.prototype = {
		resize: function(w,h,zero) {
			this.row = new haxe.ds.IntMap();
			this.nonDestructiveResize(w,h,zero);
		}
		,nonDestructiveResize: function(w,h,zero) {
			this.w = w;
			this.h = h;
			this.zero = zero;
		}
		,get: function(x,y) {
			var cursor = this.row.h[y];
			if(cursor == null) return this.zero;
			var val = cursor.h[x];
			if(val == null) return this.zero;
			return val;
		}
		,set: function(x,y,val) {
			var cursor = this.row.h[y];
			if(cursor == null) {
				cursor = new haxe.ds.IntMap();
				this.row.h[y] = cursor;
			}
			cursor.h[x] = val;
		}
		,__class__: coopy.SparseSheet
	};
	coopy.SqlColumn = $hx_exports.coopy.SqlColumn = function() {
		this.name = "";
		this.primary = false;
		this.type_value = null;
		this.type_family = null;
	};
	coopy.SqlColumn.__name__ = true;
	coopy.SqlColumn.prototype = {
		setName: function(name) {
			this.name = name;
		}
		,setPrimaryKey: function(primary) {
			this.primary = primary;
		}
		,setType: function(value,family) {
			this.type_value = value;
			this.type_family = family;
		}
		,getName: function() {
			return this.name;
		}
		,isPrimaryKey: function() {
			return this.primary;
		}
		,toString: function() {
			return (this.primary?"*":"") + this.name;
		}
		,__class__: coopy.SqlColumn
	};
	coopy.SqlCompare = $hx_exports.coopy.SqlCompare = function(db,local,remote,alt,align) {
		this.db = db;
		this.local = local;
		this.remote = remote;
		this.alt = alt;
		this.align = align;
		this.peered = false;
		this.alt_peered = false;
		if(local != null && remote != null) {
			if(this.remote.getDatabase().getNameForAttachment() != null) {
				if(this.remote.getDatabase().getNameForAttachment() != this.local.getDatabase().getNameForAttachment()) {
					local.getDatabase().getHelper().attach(db,"__peer__",this.remote.getDatabase().getNameForAttachment());
					this.peered = true;
				}
			}
		}
		if(this.alt != null && local != null) {
			if(this.alt.getDatabase().getNameForAttachment() != null) {
				if(this.alt.getDatabase().getNameForAttachment() != this.local.getDatabase().getNameForAttachment()) {
					local.getDatabase().getHelper().attach(db,"__alt__",this.alt.getDatabase().getNameForAttachment());
					this.alt_peered = true;
				}
			}
		}
	};
	coopy.SqlCompare.__name__ = true;
	coopy.SqlCompare.prototype = {
		equalArray: function(a1,a2) {
			if(a1.length != a2.length) return false;
			var _g1 = 0;
			var _g = a1.length;
			while(_g1 < _g) {
				var i = _g1++;
				if(a1[i] != a2[i]) return false;
			}
			return true;
		}
		,validateSchema: function() {
			var all_cols1 = [];
			var key_cols1 = [];
			var access_error = false;
			var pk_missing = false;
			if(this.local != null) {
				all_cols1 = this.local.getColumnNames();
				key_cols1 = this.local.getPrimaryKey();
				if(all_cols1.length == 0) access_error = true;
				if(key_cols1.length == 0) pk_missing = true;
			}
			var all_cols2 = [];
			var key_cols2 = [];
			if(this.remote != null) {
				all_cols2 = this.remote.getColumnNames();
				key_cols2 = this.remote.getPrimaryKey();
				if(all_cols2.length == 0) access_error = true;
				if(key_cols2.length == 0) pk_missing = true;
			}
			var all_cols3 = all_cols2;
			var key_cols3 = key_cols2;
			if(this.alt != null) {
				all_cols3 = this.alt.getColumnNames();
				key_cols3 = this.alt.getPrimaryKey();
				if(all_cols3.length == 0) access_error = true;
				if(key_cols3.length == 0) pk_missing = true;
			}
			if(access_error) throw "Error accessing SQL table";
			if(pk_missing) throw "sql diff not possible when primary key not available";
			var pk_change = false;
			if(this.local != null && this.remote != null) {
				if(!this.equalArray(key_cols1,key_cols2)) pk_change = true;
			}
			if(this.local != null && this.alt != null) {
				if(!this.equalArray(key_cols1,key_cols3)) pk_change = true;
			}
			if(pk_change) throw "sql diff not possible when primary key changes";
			return true;
		}
		,denull: function(x) {
			if(x == null) return -1;
			return x;
		}
		,link: function() {
			this.diff_ct++;
			var mode = this.db.get(0);
			var i0 = this.denull(this.db.get(1));
			var i1 = this.denull(this.db.get(2));
			var i2 = this.denull(this.db.get(3));
			if(i0 == -3) {
				i0 = this.at0;
				this.at0++;
			}
			if(i1 == -3) {
				i1 = this.at1;
				this.at1++;
			}
			if(i2 == -3) {
				i2 = this.at2;
				this.at2++;
			}
			var offset = 4;
			if(i0 >= 0) {
				var _g1 = 0;
				var _g = this.local.get_width();
				while(_g1 < _g) {
					var x = _g1++;
					this.local.setCellCache(x,i0,this.db.get(x + offset));
				}
				offset += this.local.get_width();
			}
			if(i1 >= 0) {
				var _g11 = 0;
				var _g2 = this.remote.get_width();
				while(_g11 < _g2) {
					var x1 = _g11++;
					this.remote.setCellCache(x1,i1,this.db.get(x1 + offset));
				}
				offset += this.remote.get_width();
			}
			if(i2 >= 0) {
				var _g12 = 0;
				var _g3 = this.alt.get_width();
				while(_g12 < _g3) {
					var x2 = _g12++;
					this.alt.setCellCache(x2,i2,this.db.get(x2 + offset));
				}
			}
			if(mode == 0 || mode == 2) {
				this.align.link(i0,i1);
				this.align.addToOrder(i0,i1);
			}
			if(this.alt != null) {
				if(mode == 1 || mode == 2) {
					this.align.reference.link(i0,i2);
					this.align.reference.addToOrder(i0,i2);
				}
			}
		}
		,linkQuery: function(query,order) {
			if(this.db.begin(query,null,order)) {
				while(this.db.read()) this.link();
				this.db.end();
			}
		}
		,where: function(txt) {
			if(txt == "") return " WHERE 1 = 0";
			return " WHERE " + txt;
		}
		,scanColumns: function(all_cols1,all_cols2,key_cols,present1,present2,align) {
			align.meta = new coopy.Alignment();
			var _g1 = 0;
			var _g = all_cols1.length;
			while(_g1 < _g) {
				var i = _g1++;
				var key = all_cols1[i];
				if(__map_reserved[key] != null?present2.existsReserved(key):present2.h.hasOwnProperty(key)) align.meta.link(i,__map_reserved[key] != null?present2.getReserved(key):present2.h[key]); else align.meta.link(i,-1);
			}
			var _g11 = 0;
			var _g2 = all_cols2.length;
			while(_g11 < _g2) {
				var i1 = _g11++;
				var key1 = all_cols2[i1];
				if(!(__map_reserved[key1] != null?present1.existsReserved(key1):present1.h.hasOwnProperty(key1))) align.meta.link(-1,i1);
			}
			align.meta.range(all_cols1.length,all_cols2.length);
			var _g3 = 0;
			while(_g3 < key_cols.length) {
				var key2 = key_cols[_g3];
				++_g3;
				var unit = new coopy.Unit(__map_reserved[key2] != null?present1.getReserved(key2):present1.h[key2],__map_reserved[key2] != null?present2.getReserved(key2):present2.h[key2]);
				align.addIndexColumns(unit);
			}
		}
		,apply: function() {
			if(this.db == null) return null;
			if(this.align == null) this.align = new coopy.Alignment();
			if(!this.validateSchema()) return null;
			var rowid_name = this.db.rowid();
			var key_cols = [];
			var data_cols = [];
			var all_cols = [];
			var all_cols1 = [];
			var all_cols2 = [];
			var all_cols3 = [];
			var common = this.local;
			if(this.local != null) {
				key_cols = this.local.getPrimaryKey();
				data_cols = this.local.getAllButPrimaryKey();
				all_cols = this.local.getColumnNames();
				all_cols1 = this.local.getColumnNames();
			}
			if(this.remote != null) {
				all_cols2 = this.remote.getColumnNames();
				if(common == null) common = this.remote;
			}
			if(this.alt != null) {
				all_cols3 = this.alt.getColumnNames();
				if(common == null) common = this.alt;
			} else all_cols3 = all_cols2;
			var all_common_cols = [];
			var data_common_cols = [];
			var present1 = new haxe.ds.StringMap();
			var present2 = new haxe.ds.StringMap();
			var present3 = new haxe.ds.StringMap();
			var present_primary = new haxe.ds.StringMap();
			var has_column_add = false;
			var _g1 = 0;
			var _g = key_cols.length;
			while(_g1 < _g) {
				var i = _g1++;
				present_primary.set(key_cols[i],i);
			}
			var _g11 = 0;
			var _g2 = all_cols1.length;
			while(_g11 < _g2) {
				var i1 = _g11++;
				var key = all_cols1[i1];
				if(__map_reserved[key] != null) present1.setReserved(key,i1); else present1.h[key] = i1;
			}
			var _g12 = 0;
			var _g3 = all_cols2.length;
			while(_g12 < _g3) {
				var i2 = _g12++;
				var key1 = all_cols2[i2];
				if(!(__map_reserved[key1] != null?present1.existsReserved(key1):present1.h.hasOwnProperty(key1))) has_column_add = true;
				if(__map_reserved[key1] != null) present2.setReserved(key1,i2); else present2.h[key1] = i2;
			}
			var _g13 = 0;
			var _g4 = all_cols3.length;
			while(_g13 < _g4) {
				var i3 = _g13++;
				var key2 = all_cols3[i3];
				if(!(__map_reserved[key2] != null?present1.existsReserved(key2):present1.h.hasOwnProperty(key2))) has_column_add = true;
				if(__map_reserved[key2] != null) present3.setReserved(key2,i3); else present3.h[key2] = i3;
				if(__map_reserved[key2] != null?present1.existsReserved(key2):present1.h.hasOwnProperty(key2)) {
					if(__map_reserved[key2] != null?present2.existsReserved(key2):present2.h.hasOwnProperty(key2)) {
						all_common_cols.push(key2);
						if(!(__map_reserved[key2] != null?present_primary.existsReserved(key2):present_primary.h.hasOwnProperty(key2))) data_common_cols.push(key2);
					}
				}
			}
			this.align.meta = new coopy.Alignment();
			var _g14 = 0;
			var _g5 = all_cols1.length;
			while(_g14 < _g5) {
				var i4 = _g14++;
				var key3 = all_cols1[i4];
				if(__map_reserved[key3] != null?present2.existsReserved(key3):present2.h.hasOwnProperty(key3)) this.align.meta.link(i4,__map_reserved[key3] != null?present2.getReserved(key3):present2.h[key3]); else this.align.meta.link(i4,-1);
			}
			var _g15 = 0;
			var _g6 = all_cols2.length;
			while(_g15 < _g6) {
				var i5 = _g15++;
				var key4 = all_cols2[i5];
				if(!(__map_reserved[key4] != null?present1.existsReserved(key4):present1.h.hasOwnProperty(key4))) this.align.meta.link(-1,i5);
			}
			this.scanColumns(all_cols1,all_cols2,key_cols,present1,present2,this.align);
			this.align.tables(this.local,this.remote);
			if(this.alt != null) {
				this.scanColumns(all_cols1,all_cols3,key_cols,present1,present3,this.align.reference);
				this.align.reference.tables(this.local,this.alt);
			}
			var sql_table1 = "";
			var sql_table2 = "";
			var sql_table3 = "";
			if(this.local != null) sql_table1 = this.local.getQuotedTableName();
			if(this.remote != null) sql_table2 = this.remote.getQuotedTableName();
			if(this.alt != null) sql_table3 = this.alt.getQuotedTableName();
			if(this.peered) {
				sql_table1 = "main." + sql_table1;
				sql_table2 = "__peer__." + sql_table2;
			}
			if(this.alt_peered) sql_table2 = "__alt__." + sql_table3;
			var sql_key_cols = "";
			var _g16 = 0;
			var _g7 = key_cols.length;
			while(_g16 < _g7) {
				var i6 = _g16++;
				if(i6 > 0) sql_key_cols += ",";
				sql_key_cols += common.getQuotedColumnName(key_cols[i6]);
			}
			var sql_all_cols = "";
			var _g17 = 0;
			var _g8 = all_common_cols.length;
			while(_g17 < _g8) {
				var i7 = _g17++;
				if(i7 > 0) sql_all_cols += ",";
				sql_all_cols += common.getQuotedColumnName(all_common_cols[i7]);
			}
			var sql_all_cols1 = "";
			var _g18 = 0;
			var _g9 = all_cols1.length;
			while(_g18 < _g9) {
				var i8 = _g18++;
				if(i8 > 0) sql_all_cols1 += ",";
				sql_all_cols1 += this.local.getQuotedColumnName(all_cols1[i8]);
			}
			var sql_all_cols2 = "";
			var _g19 = 0;
			var _g10 = all_cols2.length;
			while(_g19 < _g10) {
				var i9 = _g19++;
				if(i9 > 0) sql_all_cols2 += ",";
				sql_all_cols2 += this.remote.getQuotedColumnName(all_cols2[i9]);
			}
			var sql_all_cols3 = "";
			if(this.alt != null) {
				var _g110 = 0;
				var _g20 = all_cols3.length;
				while(_g110 < _g20) {
					var i10 = _g110++;
					if(i10 > 0) sql_all_cols3 += ",";
					sql_all_cols3 += this.alt.getQuotedColumnName(all_cols3[i10]);
				}
			}
			var sql_key_match2 = "";
			var _g111 = 0;
			var _g21 = key_cols.length;
			while(_g111 < _g21) {
				var i11 = _g111++;
				if(i11 > 0) sql_key_match2 += " AND ";
				var n = common.getQuotedColumnName(key_cols[i11]);
				sql_key_match2 += sql_table1 + "." + n + " IS " + sql_table2 + "." + n;
			}
			var sql_key_match3 = "";
			if(this.alt != null) {
				var _g112 = 0;
				var _g22 = key_cols.length;
				while(_g112 < _g22) {
					var i12 = _g112++;
					if(i12 > 0) sql_key_match3 += " AND ";
					var n1 = common.getQuotedColumnName(key_cols[i12]);
					sql_key_match3 += sql_table1 + "." + n1 + " IS " + sql_table3 + "." + n1;
				}
			}
			var sql_data_mismatch = "";
			var _g113 = 0;
			var _g23 = data_common_cols.length;
			while(_g113 < _g23) {
				var i13 = _g113++;
				if(i13 > 0) sql_data_mismatch += " OR ";
				var n2 = common.getQuotedColumnName(data_common_cols[i13]);
				sql_data_mismatch += sql_table1 + "." + n2 + " IS NOT " + sql_table2 + "." + n2;
			}
			var _g114 = 0;
			var _g24 = all_cols2.length;
			while(_g114 < _g24) {
				var i14 = _g114++;
				var key5 = all_cols2[i14];
				if(!(__map_reserved[key5] != null?present1.existsReserved(key5):present1.h.hasOwnProperty(key5))) {
					if(sql_data_mismatch != "") sql_data_mismatch += " OR ";
					var n3 = common.getQuotedColumnName(key5);
					sql_data_mismatch += sql_table2 + "." + n3 + " IS NOT NULL";
				}
			}
			if(this.alt != null) {
				var _g115 = 0;
				var _g25 = data_common_cols.length;
				while(_g115 < _g25) {
					var i15 = _g115++;
					if(sql_data_mismatch.length > 0) sql_data_mismatch += " OR ";
					var n4 = common.getQuotedColumnName(data_common_cols[i15]);
					sql_data_mismatch += sql_table1 + "." + n4 + " IS NOT " + sql_table3 + "." + n4;
				}
				var _g116 = 0;
				var _g26 = all_cols3.length;
				while(_g116 < _g26) {
					var i16 = _g116++;
					var key6 = all_cols3[i16];
					if(!(__map_reserved[key6] != null?present1.existsReserved(key6):present1.h.hasOwnProperty(key6))) {
						if(sql_data_mismatch != "") sql_data_mismatch += " OR ";
						var n5 = common.getQuotedColumnName(key6);
						sql_data_mismatch += sql_table3 + "." + n5 + " IS NOT NULL";
					}
				}
			}
			var sql_dbl_cols = "";
			var dbl_cols = [];
			var _g117 = 0;
			var _g27 = all_cols1.length;
			while(_g117 < _g27) {
				var i17 = _g117++;
				if(sql_dbl_cols != "") sql_dbl_cols += ",";
				var buf = "__coopy_" + i17;
				var n6 = common.getQuotedColumnName(all_cols1[i17]);
				sql_dbl_cols += sql_table1 + "." + n6 + " AS " + buf;
				dbl_cols.push(buf);
			}
			var _g118 = 0;
			var _g28 = all_cols2.length;
			while(_g118 < _g28) {
				var i18 = _g118++;
				if(sql_dbl_cols != "") sql_dbl_cols += ",";
				var buf1 = "__coopy_" + i18 + "b";
				var n7 = common.getQuotedColumnName(all_cols2[i18]);
				sql_dbl_cols += sql_table2 + "." + n7 + " AS " + buf1;
				dbl_cols.push(buf1);
			}
			if(this.alt != null) {
				var _g119 = 0;
				var _g29 = all_cols3.length;
				while(_g119 < _g29) {
					var i19 = _g119++;
					if(sql_dbl_cols != "") sql_dbl_cols += ",";
					var buf2 = "__coopy_" + i19 + "c";
					var n8 = common.getQuotedColumnName(all_cols3[i19]);
					sql_dbl_cols += sql_table3 + "." + n8 + " AS " + buf2;
					dbl_cols.push(buf2);
				}
			}
			var sql_order = "";
			var _g120 = 0;
			var _g30 = key_cols.length;
			while(_g120 < _g30) {
				var i20 = _g120++;
				if(i20 > 0) sql_order += ",";
				var n9 = common.getQuotedColumnName(key_cols[i20]);
				sql_order += n9;
			}
			var rowid = "-3";
			var rowid1 = "-3";
			var rowid2 = "-3";
			var rowid3 = "-3";
			if(rowid_name != null) {
				rowid = rowid_name;
				if(this.local != null) rowid1 = sql_table1 + "." + rowid_name;
				if(this.remote != null) rowid2 = sql_table2 + "." + rowid_name;
				if(this.alt != null) rowid3 = sql_table3 + "." + rowid_name;
			}
			this.at0 = 1;
			this.at1 = 1;
			this.at2 = 1;
			this.diff_ct = 0;
			if(this.remote != null) {
				var sql_inserts = "SELECT DISTINCT 0 AS __coopy_code, NULL, " + rowid + " AS rowid, NULL, " + sql_all_cols2 + " FROM " + sql_table2;
				if(this.local != null) sql_inserts += " WHERE NOT EXISTS (SELECT 1 FROM " + sql_table1 + this.where(sql_key_match2) + ")";
				var sql_inserts_order = ["__coopy_code","NULL","rowid","NULL"].concat(all_cols2);
				this.linkQuery(sql_inserts,sql_inserts_order);
			}
			if(this.alt != null) {
				var sql_inserts1 = "SELECT DISTINCT 1 AS __coopy_code, NULL, NULL, " + rowid + " AS rowid, " + sql_all_cols3 + " FROM " + sql_table3;
				if(this.local != null) sql_inserts1 += " WHERE NOT EXISTS (SELECT 1 FROM " + sql_table1 + this.where(sql_key_match3) + ")";
				var sql_inserts_order1 = ["__coopy_code","NULL","NULL","rowid"].concat(all_cols3);
				this.linkQuery(sql_inserts1,sql_inserts_order1);
			}
			if(this.local != null && this.remote != null) {
				var sql_updates = "SELECT DISTINCT 2 AS __coopy_code, " + rowid1 + " AS __coopy_rowid0, " + rowid2 + " AS __coopy_rowid1, ";
				if(this.alt != null) sql_updates += rowid3 + " AS __coopy_rowid2,"; else sql_updates += " NULL,";
				sql_updates += sql_dbl_cols + " FROM " + sql_table1;
				if(sql_table1 != sql_table2) sql_updates += " INNER JOIN " + sql_table2 + " ON " + sql_key_match2;
				if(this.alt != null && sql_table1 != sql_table3) sql_updates += " INNER JOIN " + sql_table3 + " ON " + sql_key_match3;
				sql_updates += this.where(sql_data_mismatch);
				var sql_updates_order = ["__coopy_code","__coopy_rowid0","__coopy_rowid1","__coopy_rowid2"].concat(dbl_cols);
				this.linkQuery(sql_updates,sql_updates_order);
			}
			if(this.alt == null) {
				if(this.local != null) {
					var sql_deletes = "SELECT DISTINCT 0 AS __coopy_code, " + rowid + " AS rowid, NULL, NULL, " + sql_all_cols1 + " FROM " + sql_table1;
					if(this.remote != null) sql_deletes += " WHERE NOT EXISTS (SELECT 1 FROM " + sql_table2 + this.where(sql_key_match2) + ")";
					var sql_deletes_order = ["__coopy_code","rowid","NULL","NULL"].concat(all_cols1);
					this.linkQuery(sql_deletes,sql_deletes_order);
				}
			}
			if(this.alt != null) {
				var sql_deletes1 = "SELECT 2 AS __coopy_code, " + rowid1 + " AS __coopy_rowid0, " + rowid2 + " AS __coopy_rowid1, ";
				sql_deletes1 += rowid3 + " AS __coopy_rowid2, ";
				sql_deletes1 += sql_dbl_cols;
				sql_deletes1 += " FROM " + sql_table1;
				if(this.remote != null) sql_deletes1 += " LEFT OUTER JOIN " + sql_table2 + " ON " + sql_key_match2;
				sql_deletes1 += " LEFT OUTER JOIN " + sql_table3 + " ON " + sql_key_match3;
				sql_deletes1 += " WHERE __coopy_rowid1 IS NULL OR __coopy_rowid2 IS NULL";
				var sql_deletes_order1 = ["__coopy_code","__coopy_rowid0","__coopy_rowid1","__coopy_rowid2"].concat(dbl_cols);
				this.linkQuery(sql_deletes1,sql_deletes_order1);
			}
			if(this.diff_ct == 0) this.align.markIdentical();
			return this.align;
		}
		,__class__: coopy.SqlCompare
	};
	coopy.SqlDatabase = function() { };
	coopy.SqlDatabase.__name__ = true;
	coopy.SqlDatabase.prototype = {
		__class__: coopy.SqlDatabase
	};
	coopy.SqlHelper = function() { };
	coopy.SqlHelper.__name__ = true;
	coopy.SqlHelper.prototype = {
		__class__: coopy.SqlHelper
	};
	coopy.SqlTable = $hx_exports.coopy.SqlTable = function(db,name,helper) {
		this.db = db;
		this.name = name;
		this.helper = helper;
		if(helper == null) this.helper = db.getHelper();
		this.cache = new haxe.ds.IntMap();
		this.h = -1;
		this.id2rid = null;
		this.getColumns();
	};
	coopy.SqlTable.__name__ = true;
	coopy.SqlTable.__interfaces__ = [coopy.RowStream,coopy.Meta,coopy.Table];
	coopy.SqlTable.prototype = {
		getColumns: function() {
			if(this.columns != null) return;
			if(this.db == null) return;
			this.columns = this.db.getColumns(this.name);
			this.columnNames = [];
			var _g = 0;
			var _g1 = this.columns;
			while(_g < _g1.length) {
				var col = _g1[_g];
				++_g;
				this.columnNames.push(col.getName());
			}
		}
		,getPrimaryKey: function() {
			this.getColumns();
			var result = [];
			var _g = 0;
			var _g1 = this.columns;
			while(_g < _g1.length) {
				var col = _g1[_g];
				++_g;
				if(!col.isPrimaryKey()) continue;
				result.push(col.getName());
			}
			return result;
		}
		,getAllButPrimaryKey: function() {
			this.getColumns();
			var result = [];
			var _g = 0;
			var _g1 = this.columns;
			while(_g < _g1.length) {
				var col = _g1[_g];
				++_g;
				if(col.isPrimaryKey()) continue;
				result.push(col.getName());
			}
			return result;
		}
		,getColumnNames: function() {
			this.getColumns();
			return this.columnNames;
		}
		,getQuotedTableName: function() {
			if(this.quotedTableName != null) return this.quotedTableName;
			this.quotedTableName = this.db.getQuotedTableName(this.name);
			return this.quotedTableName;
		}
		,getQuotedColumnName: function(name) {
			return this.db.getQuotedColumnName(name);
		}
		,getCell: function(x,y) {
			if(this.h >= 0) {
				y = y - 1;
				if(y >= 0) y = this.id2rid[y];
			} else if(y == 0) y = -1;
			if(y < 0) {
				this.getColumns();
				return this.columns[x].name;
			}
			var row = this.cache.h[y];
			if(row == null) {
				row = new haxe.ds.IntMap();
				this.getColumns();
				this.db.beginRow(this.name,y,this.columnNames);
				while(this.db.read()) {
					var _g1 = 0;
					var _g = this.get_width();
					while(_g1 < _g) {
						var i = _g1++;
						var v = this.db.get(i);
						row.set(i,v);
						v;
					}
				}
				this.db.end();
				{
					this.cache.h[y] = row;
					row;
				}
			}
			var this1 = this.cache.h[y];
			return this1.get(x);
		}
		,setCellCache: function(x,y,c) {
			var row = this.cache.h[y];
			if(row == null) {
				row = new haxe.ds.IntMap();
				this.getColumns();
				{
					this.cache.h[y] = row;
					row;
				}
			}
			var v = c;
			row.set(x,v);
			v;
		}
		,setCell: function(x,y,c) {
			console.log("SqlTable cannot set cells yet");
		}
		,getCellView: function() {
			return new coopy.SimpleView();
		}
		,isResizable: function() {
			return false;
		}
		,resize: function(w,h) {
			return false;
		}
		,clear: function() {
		}
		,insertOrDeleteRows: function(fate,hfate) {
			return false;
		}
		,insertOrDeleteColumns: function(fate,wfate) {
			return false;
		}
		,trimBlank: function() {
			return false;
		}
		,get_width: function() {
			this.getColumns();
			return this.columns.length;
		}
		,get_height: function() {
			if(this.h >= 0) return this.h;
			return -1;
		}
		,getData: function() {
			return null;
		}
		,clone: function() {
			return null;
		}
		,create: function() {
			return null;
		}
		,getMeta: function() {
			return this;
		}
		,alterColumns: function(columns) {
			var result = this.helper.alterColumns(this.db,this.name,columns);
			this.columns = null;
			return result;
		}
		,changeRow: function(rc) {
			if(this.helper == null) {
				console.log("No sql helper");
				return false;
			}
			if(rc.action == "+++") return this.helper.insert(this.db,this.name,rc.val); else if(rc.action == "---") return this.helper["delete"](this.db,this.name,rc.cond); else if(rc.action == "->") return this.helper.update(this.db,this.name,rc.cond,rc.val);
			return false;
		}
		,asTable: function() {
			var pct = 3;
			this.getColumns();
			var w = this.columnNames.length;
			var mt = new coopy.SimpleTable(w + 1,pct);
			mt.setCell(0,0,"@");
			mt.setCell(0,1,"type");
			mt.setCell(0,2,"key");
			var _g = 0;
			while(_g < w) {
				var x = _g++;
				var i = x + 1;
				mt.setCell(i,0,this.columnNames[x]);
				mt.setCell(i,1,this.columns[x].type_value);
				mt.setCell(i,2,this.columns[x].primary?"primary":"");
			}
			return mt;
		}
		,useForColumnChanges: function() {
			return true;
		}
		,useForRowChanges: function() {
			return true;
		}
		,cloneMeta: function(table) {
			return null;
		}
		,applyFlags: function(flags) {
			return false;
		}
		,getDatabase: function() {
			return this.db;
		}
		,getRowStream: function() {
			this.getColumns();
			this.db.begin("SELECT * FROM " + this.getQuotedTableName() + " ORDER BY ?",[this.db.rowid()],this.columnNames);
			return this;
		}
		,isNested: function() {
			return false;
		}
		,isSql: function() {
			return true;
		}
		,fetchRow: function() {
			if(this.db.read()) {
				var row = new haxe.ds.StringMap();
				var _g1 = 0;
				var _g = this.columnNames.length;
				while(_g1 < _g) {
					var i = _g1++;
					var v = this.db.get(i);
					row.set(this.columnNames[i],v);
					v;
				}
				return row;
			}
			this.db.end();
			return null;
		}
		,fetchColumns: function() {
			this.getColumns();
			return this.columnNames;
		}
		,getName: function() {
			return this.name.toString();
		}
		,__class__: coopy.SqlTable
	};
	coopy.SqlTableName = $hx_exports.coopy.SqlTableName = function(name,prefix) {
		if(prefix == null) prefix = "";
		if(name == null) name = "";
		this.name = name;
		this.prefix = prefix;
	};
	coopy.SqlTableName.__name__ = true;
	coopy.SqlTableName.prototype = {
		toString: function() {
			if(this.prefix == "") return this.name;
			return this.prefix + "." + this.name;
		}
		,__class__: coopy.SqlTableName
	};
	coopy.SqlTables = $hx_exports.coopy.SqlTables = function(db,flags) {
		this.db = db;
		var helper = this.db.getHelper();
		var names = helper.getTableNames(db);
		var allowed = null;
		var count = names.length;
		if(flags.tables != null) {
			allowed = new haxe.ds.StringMap();
			var _g = 0;
			var _g1 = flags.tables;
			while(_g < _g1.length) {
				var name = _g1[_g];
				++_g;
				if(__map_reserved[name] != null) allowed.setReserved(name,true); else allowed.h[name] = true;
			}
			count = 0;
			var _g2 = 0;
			while(_g2 < names.length) {
				var name1 = names[_g2];
				++_g2;
				if(__map_reserved[name1] != null?allowed.existsReserved(name1):allowed.h.hasOwnProperty(name1)) count++;
			}
		}
		this.t = new coopy.SimpleTable(2,count + 1);
		this.t.setCell(0,0,"name");
		this.t.setCell(1,0,"table");
		var v = this.t.getCellView();
		var at = 1;
		var _g3 = 0;
		while(_g3 < names.length) {
			var name2 = names[_g3];
			++_g3;
			if(allowed != null) {
				if(!(__map_reserved[name2] != null?allowed.existsReserved(name2):allowed.h.hasOwnProperty(name2))) continue;
			}
			this.t.setCell(0,at,name2);
			this.t.setCell(1,at,v.wrapTable(new coopy.SqlTable(db,new coopy.SqlTableName(name2))));
			at++;
		}
	};
	coopy.SqlTables.__name__ = true;
	coopy.SqlTables.__interfaces__ = [coopy.Table];
	coopy.SqlTables.prototype = {
		getCell: function(x,y) {
			return this.t.getCell(x,y);
		}
		,setCell: function(x,y,c) {
		}
		,getCellView: function() {
			return this.t.getCellView();
		}
		,isResizable: function() {
			return false;
		}
		,resize: function(w,h) {
			return false;
		}
		,clear: function() {
		}
		,insertOrDeleteRows: function(fate,hfate) {
			return false;
		}
		,insertOrDeleteColumns: function(fate,wfate) {
			return false;
		}
		,trimBlank: function() {
			return false;
		}
		,get_width: function() {
			return this.t.get_width();
		}
		,get_height: function() {
			return this.t.get_height();
		}
		,getData: function() {
			return null;
		}
		,clone: function() {
			return null;
		}
		,create: function() {
			return null;
		}
		,getMeta: function() {
			return new coopy.SimpleMeta(this,true,true);
		}
		,__class__: coopy.SqlTables
	};
	coopy.SqliteHelper = $hx_exports.coopy.SqliteHelper = function() {
	};
	coopy.SqliteHelper.__name__ = true;
	coopy.SqliteHelper.__interfaces__ = [coopy.SqlHelper];
	coopy.SqliteHelper.prototype = {
		getTableNames: function(db) {
			var q = "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name";
			if(!db.begin(q,null,["name"])) return null;
			var names = [];
			while(db.read()) names.push(db.get(0));
			db.end();
			return names;
		}
		,countRows: function(db,name) {
			var q = "SELECT COUNT(*) AS ct FROM " + db.getQuotedTableName(name);
			if(!db.begin(q,null,["ct"])) return -1;
			var ct = -1;
			while(db.read()) ct = db.get(0);
			db.end();
			return ct;
		}
		,getRowIDs: function(db,name) {
			var result = [];
			var q = "SELECT ROWID AS r FROM " + db.getQuotedTableName(name) + " ORDER BY ROWID";
			if(!db.begin(q,null,["r"])) return null;
			while(db.read()) {
				var c = db.get(0);
				result.push(c);
			}
			db.end();
			return result;
		}
		,update: function(db,name,conds,vals) {
			var q = "UPDATE " + db.getQuotedTableName(name) + " SET ";
			var lst = [];
			var $it0 = vals.keys();
			while( $it0.hasNext() ) {
				var k = $it0.next();
				if(lst.length > 0) q += ", ";
				q += db.getQuotedColumnName(k);
				q += " = ?";
				lst.push(__map_reserved[k] != null?vals.getReserved(k):vals.h[k]);
			}
			var val_len = lst.length;
			q += " WHERE ";
			var $it1 = conds.keys();
			while( $it1.hasNext() ) {
				var k1 = $it1.next();
				if(lst.length > val_len) q += " and ";
				q += db.getQuotedColumnName(k1);
				q += " IS ?";
				lst.push(__map_reserved[k1] != null?conds.getReserved(k1):conds.h[k1]);
			}
			if(!db.begin(q,lst,[])) {
				console.log("Problem with database update");
				return false;
			}
			db.end();
			return true;
		}
		,'delete': function(db,name,conds) {
			var q = "DELETE FROM " + db.getQuotedTableName(name) + " WHERE ";
			var lst = [];
			var $it0 = conds.keys();
			while( $it0.hasNext() ) {
				var k = $it0.next();
				if(lst.length > 0) q += " and ";
				q += db.getQuotedColumnName(k);
				q += " = ?";
				lst.push(__map_reserved[k] != null?conds.getReserved(k):conds.h[k]);
			}
			if(!db.begin(q,lst,[])) {
				console.log("Problem with database delete");
				return false;
			}
			db.end();
			return true;
		}
		,insert: function(db,name,vals) {
			var q = "INSERT INTO " + db.getQuotedTableName(name) + " (";
			var lst = [];
			var $it0 = vals.keys();
			while( $it0.hasNext() ) {
				var k = $it0.next();
				if(lst.length > 0) q += ",";
				q += db.getQuotedColumnName(k);
				lst.push(__map_reserved[k] != null?vals.getReserved(k):vals.h[k]);
			}
			q += ") VALUES(";
			var need_comma = false;
			var $it1 = vals.keys();
			while( $it1.hasNext() ) {
				var k1 = $it1.next();
				if(need_comma) q += ",";
				q += "?";
				need_comma = true;
			}
			q += ")";
			if(!db.begin(q,lst,[])) {
				console.log("Problem with database insert");
				return false;
			}
			db.end();
			return true;
		}
		,attach: function(db,tag,resource_name) {
			var tag_present = false;
			var tag_correct = false;
			var result = [];
			var q = "PRAGMA database_list";
			if(!db.begin(q,null,["seq","name","file"])) return false;
			while(db.read()) {
				var name = db.get(1);
				if(name == tag) {
					tag_present = true;
					var file = db.get(2);
					if(file == resource_name) tag_correct = true;
				}
			}
			db.end();
			if(tag_present) {
				if(tag_correct) return true;
				if(!db.begin("DETACH `" + tag + "`",null,[])) {
					console.log("Failed to detach " + tag);
					return false;
				}
				db.end();
			}
			if(!db.begin("ATTACH ? AS `" + tag + "`",[resource_name],[])) {
				console.log("Failed to attach " + resource_name + " as " + tag);
				return false;
			}
			db.end();
			return true;
		}
		,columnListSql: function(x) {
			return x.join(",");
		}
		,fetchSchema: function(db,name) {
			var tname = db.getQuotedTableName(name);
			var query = "select sql from sqlite_master where name = '" + tname + "'";
			if(!db.begin(query,null,["sql"])) {
				console.log("Cannot find schema for table " + tname);
				return null;
			}
			var sql = "";
			if(db.read()) sql = db.get(0);
			db.end();
			return sql;
		}
		,splitSchema: function(db,name,sql) {
			var preamble = "";
			var parts = [];
			var double_quote = false;
			var single_quote = false;
			var token = "";
			var nesting = 0;
			var _g1 = 0;
			var _g = sql.length;
			while(_g1 < _g) {
				var i = _g1++;
				var ch = sql.charAt(i);
				if(double_quote || single_quote) {
					if(double_quote) {
						if(ch == "\"") double_quote = false;
					}
					if(single_quote) {
						if(ch == "'") single_quote = false;
					}
					token += ch;
					continue;
				}
				var brk = false;
				if(ch == "(") {
					nesting++;
					if(nesting == 1) brk = true;
				} else if(ch == ")") {
					nesting--;
					if(nesting == 0) brk = true;
				}
				if(ch == ",") {
					brk = true;
					if(nesting == 1) {
					}
				}
				if(brk) {
					if(token.charAt(0) == " ") token = HxOverrides.substr(token,1,token.length);
					if(preamble == "") preamble = token; else parts.push(token);
					token = "";
				} else token += ch;
			}
			var cols = db.getColumns(name);
			var name2part = new haxe.ds.StringMap();
			var name2col = new haxe.ds.StringMap();
			var _g11 = 0;
			var _g2 = cols.length;
			while(_g11 < _g2) {
				var i1 = _g11++;
				var col = cols[i1];
				name2part.set(col.name,parts[i1]);
				name2col.set(col.name,cols[i1]);
			}
			return { preamble : preamble, parts : parts, name2part : name2part, columns : cols, name2column : name2col};
		}
		,exec: function(db,query) {
			if(!db.begin(query)) {
				console.log("database problem");
				return false;
			}
			db.end();
			return true;
		}
		,alterColumns: function(db,name,columns) {
			var notBlank = function(x) {
				if(x == null || x == "" || x == "null") return false;
				return true;
			};
			var sql = this.fetchSchema(db,name);
			var schema = this.splitSchema(db,name,sql);
			var parts = schema.parts;
			var nparts = [];
			var new_column_list = [];
			var ins_column_list = [];
			var sel_column_list = [];
			var meta = schema.columns;
			var _g1 = 0;
			var _g = columns.length;
			while(_g1 < _g) {
				var i = _g1++;
				var c = columns[i];
				if(c.name != null) {
					if(c.prevName != null) {
						sel_column_list.push(c.prevName);
						ins_column_list.push(c.name);
					}
					var orig_type = "";
					var orig_primary = false;
					if(schema.name2column.exists(c.name)) {
						var m = schema.name2column.get(c.name);
						orig_type = m.type_value;
						orig_primary = m.primary;
					}
					var next_type = orig_type;
					var next_primary = orig_primary;
					if(c.props != null) {
						var _g2 = 0;
						var _g3 = c.props;
						while(_g2 < _g3.length) {
							var p = _g3[_g2];
							++_g2;
							if(p.name == "type") next_type = p.val;
							if(p.name == "key") next_primary = "" + Std.string(p.val) == "primary";
						}
					}
					var part = "" + c.name;
					if(notBlank(next_type)) part += " " + next_type;
					if(next_primary) part += " PRIMARY KEY";
					nparts.push(part);
					new_column_list.push(c.name);
				}
			}
			if(!this.exec(db,"BEGIN TRANSACTION")) return false;
			var c1 = this.columnListSql(ins_column_list);
			var tname = db.getQuotedTableName(name);
			if(!this.exec(db,"CREATE TEMPORARY TABLE __coopy_backup(" + c1 + ")")) return false;
			if(!this.exec(db,"INSERT INTO __coopy_backup (" + c1 + ") SELECT " + c1 + " FROM " + tname)) return false;
			if(!this.exec(db,"DROP TABLE " + tname)) return false;
			if(!this.exec(db,schema.preamble + "(" + nparts.join(", ") + ")")) return false;
			if(!this.exec(db,"INSERT INTO " + tname + " (" + c1 + ") SELECT " + c1 + " FROM __coopy_backup")) return false;
			if(!this.exec(db,"DROP TABLE __coopy_backup")) return false;
			if(!this.exec(db,"COMMIT")) return false;
			return true;
		}
		,__class__: coopy.SqliteHelper
	};
	coopy.TableComparisonState = $hx_exports.coopy.TableComparisonState = function() {
		this.reset();
	};
	coopy.TableComparisonState.__name__ = true;
	coopy.TableComparisonState.prototype = {
		reset: function() {
			this.completed = false;
			this.run_to_completion = true;
			this.is_equal_known = false;
			this.is_equal = false;
			this.has_same_columns = false;
			this.has_same_columns_known = false;
			this.compare_flags = null;
			this.alignment = null;
			this.children = null;
			this.child_order = null;
		}
		,getMeta: function() {
			if(this.p != null && this.p_meta == null) this.p_meta = this.p.getMeta();
			if(this.a != null && this.a_meta == null) this.a_meta = this.a.getMeta();
			if(this.b != null && this.b_meta == null) this.b_meta = this.b.getMeta();
		}
		,__class__: coopy.TableComparisonState
	};
	coopy.TableDiff = $hx_exports.coopy.TableDiff = function(align,flags) {
		this.align = align;
		this.flags = flags;
		this.builder = null;
		this.preserve_columns = false;
	};
	coopy.TableDiff.__name__ = true;
	coopy.TableDiff.prototype = {
		setCellBuilder: function(builder) {
			this.builder = builder;
		}
		,getSeparator: function(t,t2,root) {
			var sep = root;
			var w = t.get_width();
			var h = t.get_height();
			var view = t.getCellView();
			var _g = 0;
			while(_g < h) {
				var y = _g++;
				var _g1 = 0;
				while(_g1 < w) {
					var x = _g1++;
					var txt = view.toString(t.getCell(x,y));
					if(txt == null) continue;
					while(txt.indexOf(sep) >= 0) sep = "-" + sep;
				}
			}
			if(t2 != null) {
				w = t2.get_width();
				h = t2.get_height();
				var _g2 = 0;
				while(_g2 < h) {
					var y1 = _g2++;
					var _g11 = 0;
					while(_g11 < w) {
						var x1 = _g11++;
						var txt1 = view.toString(t2.getCell(x1,y1));
						if(txt1 == null) continue;
						while(txt1.indexOf(sep) >= 0) sep = "-" + sep;
					}
				}
			}
			return sep;
		}
		,quoteForDiff: function(v,d) {
			var nil = "NULL";
			if(v.equals(d,null)) return nil;
			var str = v.toString(d);
			var score = 0;
			var _g1 = 0;
			var _g = str.length;
			while(_g1 < _g) {
				var i = _g1++;
				if(HxOverrides.cca(str,score) != 95) break;
				score++;
			}
			if(HxOverrides.substr(str,score,null) == nil) str = "_" + str;
			return str;
		}
		,isReordered: function(m,ct) {
			var reordered = false;
			var l = -1;
			var r = -1;
			var _g = 0;
			while(_g < ct) {
				var i = _g++;
				var unit = m.h[i];
				if(unit == null) continue;
				if(unit.l >= 0) {
					if(unit.l < l) {
						reordered = true;
						break;
					}
					l = unit.l;
				}
				if(unit.r >= 0) {
					if(unit.r < r) {
						reordered = true;
						break;
					}
					r = unit.r;
				}
			}
			return reordered;
		}
		,spreadContext: function(units,del,active) {
			if(del > 0 && active != null) {
				var mark = -del - 1;
				var skips = 0;
				var _g1 = 0;
				var _g = units.length;
				while(_g1 < _g) {
					var i = _g1++;
					if(active[i] == -3) {
						skips++;
						continue;
					}
					if(active[i] == 0 || active[i] == 3) {
						if(i - mark <= del + skips) active[i] = 2; else if(i - mark == del + 1 + skips) active[i] = 3;
					} else if(active[i] == 1) {
						mark = i;
						skips = 0;
					}
				}
				mark = units.length + del + 1;
				skips = 0;
				var _g11 = 0;
				var _g2 = units.length;
				while(_g11 < _g2) {
					var j = _g11++;
					var i1 = units.length - 1 - j;
					if(active[i1] == -3) {
						skips++;
						continue;
					}
					if(active[i1] == 0 || active[i1] == 3) {
						if(mark - i1 <= del + skips) active[i1] = 2; else if(mark - i1 == del + 1 + skips) active[i1] = 3;
					} else if(active[i1] == 1) {
						mark = i1;
						skips = 0;
					}
				}
			}
		}
		,setIgnore: function(ignore,idx_ignore,tab,r_header) {
			var v = tab.getCellView();
			if(tab.get_height() >= r_header) {
				var _g1 = 0;
				var _g = tab.get_width();
				while(_g1 < _g) {
					var i = _g1++;
					var name = v.toString(tab.getCell(i,r_header));
					if(!(__map_reserved[name] != null?ignore.existsReserved(name):ignore.h.hasOwnProperty(name))) continue;
					idx_ignore.h[i] = true;
				}
			}
		}
		,countActive: function(active) {
			var ct = 0;
			var showed_dummy = false;
			var _g1 = 0;
			var _g = active.length;
			while(_g1 < _g) {
				var i = _g1++;
				var publish = active[i] > 0;
				var dummy = active[i] == 3;
				if(dummy && showed_dummy) continue;
				if(!publish) continue;
				showed_dummy = dummy;
				ct++;
			}
			return ct;
		}
		,reset: function() {
			this.has_parent = false;
			this.rp_header = this.ra_header = this.rb_header = 0;
			this.is_index_p = new haxe.ds.IntMap();
			this.is_index_a = new haxe.ds.IntMap();
			this.is_index_b = new haxe.ds.IntMap();
			this.row_map = new haxe.ds.IntMap();
			this.col_map = new haxe.ds.IntMap();
			this.show_rc_numbers = false;
			this.row_moves = null;
			this.col_moves = null;
			this.allow_insert = this.allow_delete = this.allow_update = true;
			this.sep = "";
			this.conflict_sep = "";
			this.top_line_done = false;
			this.diff_found = false;
			this.schema_diff_found = false;
		}
		,setupTables: function() {
			this.order = this.align.toOrder();
			this.row_units = this.order.getList();
			this.has_parent = this.align.reference != null;
			if(this.has_parent) {
				this.p = this.align.getSource();
				this.a = this.align.reference.getTarget();
				this.b = this.align.getTarget();
				this.rp_header = this.align.reference.meta.getSourceHeader();
				this.ra_header = this.align.reference.meta.getTargetHeader();
				this.rb_header = this.align.meta.getTargetHeader();
				if(this.align.getIndexColumns() != null) {
					var _g = 0;
					var _g1 = this.align.getIndexColumns();
					while(_g < _g1.length) {
						var p2b = _g1[_g];
						++_g;
						if(p2b.l >= 0) this.is_index_p.h[p2b.l] = true;
						if(p2b.r >= 0) this.is_index_b.h[p2b.r] = true;
					}
				}
				if(this.align.reference.getIndexColumns() != null) {
					var _g2 = 0;
					var _g11 = this.align.reference.getIndexColumns();
					while(_g2 < _g11.length) {
						var p2a = _g11[_g2];
						++_g2;
						if(p2a.l >= 0) this.is_index_p.h[p2a.l] = true;
						if(p2a.r >= 0) this.is_index_a.h[p2a.r] = true;
					}
				}
			} else {
				this.a = this.align.getSource();
				this.b = this.align.getTarget();
				this.p = this.a;
				this.ra_header = this.align.meta.getSourceHeader();
				this.rp_header = this.ra_header;
				this.rb_header = this.align.meta.getTargetHeader();
				if(this.align.getIndexColumns() != null) {
					var _g3 = 0;
					var _g12 = this.align.getIndexColumns();
					while(_g3 < _g12.length) {
						var a2b = _g12[_g3];
						++_g3;
						if(a2b.l >= 0) this.is_index_a.h[a2b.l] = true;
						if(a2b.r >= 0) this.is_index_b.h[a2b.r] = true;
					}
				}
			}
			this.allow_insert = this.flags.allowInsert();
			this.allow_delete = this.flags.allowDelete();
			this.allow_update = this.flags.allowUpdate();
			var common = this.a;
			if(common == null) common = this.b;
			if(common == null) common = this.p;
			this.v = common.getCellView();
			this.builder.setView(this.v);
			this.nested = false;
			var meta = common.getMeta();
			if(meta != null) this.nested = meta.isNested();
			this.nesting_present = false;
		}
		,scanActivity: function() {
			this.active_row = [];
			this.active_column = null;
			if(!this.flags.show_unchanged) {
				var _g1 = 0;
				var _g = this.row_units.length;
				while(_g1 < _g) {
					var i = _g1++;
					this.active_row[this.row_units.length - 1 - i] = 0;
				}
			}
			if(!this.flags.show_unchanged_columns) {
				this.active_column = [];
				var _g11 = 0;
				var _g2 = this.column_units.length;
				while(_g11 < _g2) {
					var i1 = _g11++;
					var v = 0;
					var unit = this.column_units[i1];
					if(unit.l >= 0 && this.is_index_a.h[unit.l]) v = 1;
					if(unit.r >= 0 && this.is_index_b.h[unit.r]) v = 1;
					if(unit.p >= 0 && this.is_index_p.h[unit.p]) v = 1;
					this.active_column[i1] = v;
				}
			}
		}
		,setupColumns: function() {
			var column_order = this.align.meta.toOrder();
			this.column_units = column_order.getList();
			var ignore = this.flags.getIgnoredColumns();
			if(ignore != null) {
				var p_ignore = new haxe.ds.IntMap();
				var a_ignore = new haxe.ds.IntMap();
				var b_ignore = new haxe.ds.IntMap();
				this.setIgnore(ignore,p_ignore,this.p,this.rp_header);
				this.setIgnore(ignore,a_ignore,this.a,this.ra_header);
				this.setIgnore(ignore,b_ignore,this.b,this.rb_header);
				var ncolumn_units = [];
				var _g1 = 0;
				var _g = this.column_units.length;
				while(_g1 < _g) {
					var j = _g1++;
					var cunit = this.column_units[j];
					if(p_ignore.h.hasOwnProperty(cunit.p) || a_ignore.h.hasOwnProperty(cunit.l) || b_ignore.h.hasOwnProperty(cunit.r)) continue;
					ncolumn_units.push(cunit);
				}
				this.column_units = ncolumn_units;
			}
		}
		,setupMoves: function() {
			if(this.flags.ordered) {
				this.row_moves = new haxe.ds.IntMap();
				var moves = coopy.Mover.moveUnits(this.row_units);
				var _g1 = 0;
				var _g = moves.length;
				while(_g1 < _g) {
					var i = _g1++;
					{
						this.row_moves.h[moves[i]] = i;
						i;
					}
				}
				this.col_moves = new haxe.ds.IntMap();
				moves = coopy.Mover.moveUnits(this.column_units);
				var _g11 = 0;
				var _g2 = moves.length;
				while(_g11 < _g2) {
					var i1 = _g11++;
					{
						this.col_moves.h[moves[i1]] = i1;
						i1;
					}
				}
			}
		}
		,scanSchema: function() {
			this.schema = [];
			this.have_schema = false;
			var _g1 = 0;
			var _g = this.column_units.length;
			while(_g1 < _g) {
				var j = _g1++;
				var cunit = this.column_units[j];
				var reordered = false;
				if(this.flags.ordered) {
					if(this.col_moves.h.hasOwnProperty(j)) reordered = true;
					if(reordered) this.show_rc_numbers = true;
				}
				var act = "";
				if(cunit.r >= 0 && cunit.lp() == -1) {
					this.have_schema = true;
					act = "+++";
					if(this.active_column != null) {
						if(this.allow_update) this.active_column[j] = 1;
					}
				}
				if(cunit.r < 0 && cunit.lp() >= 0) {
					this.have_schema = true;
					act = "---";
					if(this.active_column != null) {
						if(this.allow_update) this.active_column[j] = 1;
					}
				}
				if(cunit.r >= 0 && cunit.lp() >= 0) {
					if(this.p.get_height() >= this.rp_header && this.b.get_height() >= this.rb_header) {
						var pp = this.p.getCell(cunit.lp(),this.rp_header);
						var bb = this.b.getCell(cunit.r,this.rb_header);
						if(!this.isEqual(this.v,pp,bb)) {
							this.have_schema = true;
							act = "(";
							act += this.v.toString(pp);
							act += ")";
							if(this.active_column != null) this.active_column[j] = 1;
						}
					}
				}
				if(reordered) {
					act = ":" + act;
					this.have_schema = true;
					if(this.active_column != null) this.active_column = null;
				}
				this.schema.push(act);
			}
		}
		,checkRcNumbers: function(w,h) {
			if(!this.show_rc_numbers) {
				if(this.flags.always_show_order) this.show_rc_numbers = true; else if(this.flags.ordered) {
					this.show_rc_numbers = this.isReordered(this.row_map,h);
					if(!this.show_rc_numbers) this.show_rc_numbers = this.isReordered(this.col_map,w);
				}
			}
		}
		,addRcNumbers: function(output) {
			var admin_w = 1;
			if(this.show_rc_numbers && !this.flags.never_show_order) {
				admin_w++;
				var target = [];
				var _g1 = 0;
				var _g = output.get_width();
				while(_g1 < _g) {
					var i = _g1++;
					target.push(i + 1);
				}
				output.insertOrDeleteColumns(target,output.get_width() + 1);
				var _g11 = 0;
				var _g2 = output.get_height();
				while(_g11 < _g2) {
					var i1 = _g11++;
					var unit = this.row_map.h[i1];
					if(unit == null) {
						output.setCell(0,i1,"");
						continue;
					}
					output.setCell(0,i1,this.builder.links(unit,true));
				}
				target = [];
				var _g12 = 0;
				var _g3 = output.get_height();
				while(_g12 < _g3) {
					var i2 = _g12++;
					target.push(i2 + 1);
				}
				output.insertOrDeleteRows(target,output.get_height() + 1);
				var _g13 = 1;
				var _g4 = output.get_width();
				while(_g13 < _g4) {
					var i3 = _g13++;
					var unit1 = this.col_map.h[i3 - 1];
					if(unit1 == null) {
						output.setCell(i3,0,"");
						continue;
					}
					output.setCell(i3,0,this.builder.links(unit1,false));
				}
				output.setCell(0,0,this.builder.marker("@:@"));
			}
			return admin_w;
		}
		,elideColumns: function(output,admin_w) {
			if(this.active_column != null) {
				var all_active = true;
				var _g1 = 0;
				var _g = this.active_column.length;
				while(_g1 < _g) {
					var i = _g1++;
					if(this.active_column[i] == 0) {
						all_active = false;
						break;
					}
				}
				if(!all_active) {
					var fate = [];
					var _g2 = 0;
					while(_g2 < admin_w) {
						var i1 = _g2++;
						fate.push(i1);
					}
					var at = admin_w;
					var ct = 0;
					var dots = [];
					var _g11 = 0;
					var _g3 = this.active_column.length;
					while(_g11 < _g3) {
						var i2 = _g11++;
						var off = this.active_column[i2] == 0;
						if(off) ct = ct + 1; else ct = 0;
						if(off && ct > 1) fate.push(-1); else {
							if(off) dots.push(at);
							fate.push(at);
							at++;
						}
					}
					output.insertOrDeleteColumns(fate,at);
					var _g4 = 0;
					while(_g4 < dots.length) {
						var d = dots[_g4];
						++_g4;
						var _g21 = 0;
						var _g12 = output.get_height();
						while(_g21 < _g12) {
							var j = _g21++;
							output.setCell(d,j,this.builder.marker("..."));
						}
					}
				}
			}
		}
		,addSchema: function(output) {
			if(this.have_schema) {
				var at = output.get_height();
				output.resize(this.column_units.length + 1,at + 1);
				output.setCell(0,at,this.builder.marker("!"));
				var _g1 = 0;
				var _g = this.column_units.length;
				while(_g1 < _g) {
					var j = _g1++;
					output.setCell(j + 1,at,this.v.toDatum(this.schema[j]));
				}
				this.schema_diff_found = true;
			}
		}
		,addHeader: function(output) {
			if(this.flags.always_show_header) {
				var at = output.get_height();
				output.resize(this.column_units.length + 1,at + 1);
				output.setCell(0,at,this.builder.marker("@@"));
				var _g1 = 0;
				var _g = this.column_units.length;
				while(_g1 < _g) {
					var j = _g1++;
					var cunit = this.column_units[j];
					if(cunit.r >= 0) {
						if(this.b.get_height() != 0) output.setCell(j + 1,at,this.b.getCell(cunit.r,this.rb_header));
					} else if(cunit.l >= 0) {
						if(this.a.get_height() != 0) output.setCell(j + 1,at,this.a.getCell(cunit.l,this.ra_header));
					} else if(cunit.lp() >= 0) {
						if(this.p.get_height() != 0) output.setCell(j + 1,at,this.p.getCell(cunit.lp(),this.rp_header));
					}
					this.col_map.h[j + 1] = cunit;
				}
				this.top_line_done = true;
			}
		}
		,checkMeta: function(t,meta) {
			if(meta.get_width() != t.get_width() + 1) return false;
			if(meta.get_width() == 0 || meta.get_height() == 0) return false;
			return true;
		}
		,getMetaTable: function(t) {
			if(t == null) return null;
			var meta = t.getMeta();
			if(meta == null) return null;
			return meta.asTable();
		}
		,addMeta: function(output) {
			var a_meta;
			var b_meta;
			var p_meta;
			a_meta = this.getMetaTable(this.a);
			b_meta = this.getMetaTable(this.b);
			p_meta = this.getMetaTable(this.p);
			if(a_meta == null || b_meta == null || p_meta == null) return false;
			if(!this.checkMeta(this.a,a_meta)) return false;
			if(!this.checkMeta(this.b,b_meta)) return false;
			if(!this.checkMeta(this.p,p_meta)) return false;
			if(!this.flags.show_meta) return false;
			var meta_diff = new coopy.SimpleTable(0,0);
			var meta_flags = new coopy.CompareFlags();
			meta_flags.addPrimaryKey("@@");
			meta_flags.addPrimaryKey("@");
			meta_flags.unchanged_column_context = 65536;
			meta_flags.unchanged_context = 0;
			var meta_align = coopy.Coopy.compareTables3(a_meta == p_meta?null:p_meta,a_meta,b_meta,meta_flags).align();
			var td = new coopy.TableDiff(meta_align,meta_flags);
			td.preserve_columns = true;
			td.hilite(meta_diff);
			if(td.hasDifference()) {
				var h = output.get_height();
				var dh = meta_diff.get_height();
				var offset;
				if(td.hasSchemaDifference()) offset = 2; else offset = 1;
				output.resize(output.get_width(),h + dh - offset);
				var v = meta_diff.getCellView();
				var _g = offset;
				while(_g < dh) {
					var y = _g++;
					var _g2 = 1;
					var _g1 = meta_diff.get_width();
					while(_g2 < _g1) {
						var x = _g2++;
						var c = meta_diff.getCell(x,y);
						if(x == 1) c = "@" + v.toString(c) + "@" + v.toString(meta_diff.getCell(0,y));
						output.setCell(x - 1,h + y - offset,c);
					}
				}
				if(this.active_column != null) {
					if(td.active_column.length == meta_diff.get_width()) {
						var _g11 = 1;
						var _g3 = meta_diff.get_width();
						while(_g11 < _g3) {
							var i = _g11++;
							if(td.active_column[i] >= 0) this.active_column[i - 1] = 1;
						}
					}
				}
			}
			return false;
		}
		,refineActivity: function() {
			this.spreadContext(this.row_units,this.flags.unchanged_context,this.active_row);
			this.spreadContext(this.column_units,this.flags.unchanged_column_context,this.active_column);
			if(this.active_column != null) {
				var _g1 = 0;
				var _g = this.column_units.length;
				while(_g1 < _g) {
					var i = _g1++;
					if(this.active_column[i] == 3) this.active_column[i] = 0;
				}
			}
		}
		,normalizeString: function(v,str) {
			if(str == null) return str;
			if(!(this.flags.ignore_whitespace || this.flags.ignore_case)) return str;
			var txt = v.toString(str);
			if(this.flags.ignore_whitespace) txt = StringTools.trim(txt);
			if(this.flags.ignore_case) txt = txt.toLowerCase();
			return txt;
		}
		,isEqual: function(v,aa,bb) {
			if(this.flags.ignore_whitespace || this.flags.ignore_case) return this.normalizeString(v,aa) == this.normalizeString(v,bb);
			return v.equals(aa,bb);
		}
		,checkNesting: function(v,have_ll,ll,have_rr,rr,have_pp,pp,x,y) {
			var all_tables = true;
			if(have_ll) all_tables = all_tables && v.isTable(ll);
			if(have_rr) all_tables = all_tables && v.isTable(rr);
			if(have_pp) all_tables = all_tables && v.isTable(pp);
			if(!all_tables) return [ll,rr,pp];
			var ll_table = null;
			var rr_table = null;
			var pp_table = null;
			if(have_ll) ll_table = v.getTable(ll);
			if(have_rr) rr_table = v.getTable(rr);
			if(have_pp) pp_table = v.getTable(pp);
			var compare = false;
			var comp = new coopy.TableComparisonState();
			comp.a = ll_table;
			comp.b = rr_table;
			comp.p = pp_table;
			comp.compare_flags = this.flags;
			comp.getMeta();
			var key = null;
			if(comp.a_meta != null) key = comp.a_meta.getName();
			if(key == null && comp.b_meta != null) key = comp.b_meta.getName();
			if(key == null) key = x + "_" + y;
			if(this.align.comp != null) {
				if(this.align.comp.children == null) {
					this.align.comp.children = new haxe.ds.StringMap();
					this.align.comp.child_order = [];
					compare = true;
				} else compare = !this.align.comp.children.exists(key);
			}
			if(compare) {
				this.nesting_present = true;
				this.align.comp.children.set(key,comp);
				this.align.comp.child_order.push(key);
				var ct = new coopy.CompareTable(comp);
				ct.align();
			} else comp = this.align.comp.children.get(key);
			var ll_out = null;
			var rr_out = null;
			var pp_out = null;
			if(comp.alignment.isMarkedAsIdentical() || have_ll && !have_rr || have_rr && !have_ll) {
				ll_out = "[" + key + "]";
				rr_out = ll_out;
				pp_out = ll_out;
			} else {
				if(ll != null) ll_out = "[a." + key + "]";
				if(rr != null) rr_out = "[b." + key + "]";
				if(pp != null) pp_out = "[p." + key + "]";
			}
			return [ll_out,rr_out,pp_out];
		}
		,scanRow: function(unit,output,at,i) {
			var _g1 = 0;
			var _g = this.column_units.length;
			while(_g1 < _g) {
				var j = _g1++;
				var cunit = this.column_units[j];
				var pp = null;
				var ll = null;
				var rr = null;
				var dd = null;
				var dd_to = null;
				var have_dd_to = false;
				var dd_to_alt = null;
				var have_dd_to_alt = false;
				var have_pp = false;
				var have_ll = false;
				var have_rr = false;
				if(cunit.p >= 0 && unit.p >= 0) {
					pp = this.p.getCell(cunit.p,unit.p);
					have_pp = true;
				}
				if(cunit.l >= 0 && unit.l >= 0) {
					ll = this.a.getCell(cunit.l,unit.l);
					have_ll = true;
				}
				if(cunit.r >= 0 && unit.r >= 0) {
					rr = this.b.getCell(cunit.r,unit.r);
					have_rr = true;
					if((have_pp?cunit.p:cunit.l) < 0) {
						if(rr != null) {
							if(this.v.toString(rr) != "") {
								if(this.flags.allowUpdate()) this.have_addition = true;
							}
						}
					}
				}
				if(this.nested) {
					var ndiff = this.checkNesting(this.v,have_ll,ll,have_rr,rr,have_pp,pp,i,j);
					ll = ndiff[0];
					rr = ndiff[1];
					pp = ndiff[2];
				}
				if(have_pp) {
					if(!have_rr) dd = pp; else if(this.isEqual(this.v,pp,rr)) dd = ll; else {
						dd = pp;
						dd_to = rr;
						have_dd_to = true;
						if(!this.isEqual(this.v,pp,ll)) {
							if(!this.isEqual(this.v,pp,rr)) {
								dd_to_alt = ll;
								have_dd_to_alt = true;
							}
						}
					}
				} else if(have_ll) {
					if(!have_rr) dd = ll; else if(this.isEqual(this.v,ll,rr)) dd = ll; else {
						dd = ll;
						dd_to = rr;
						have_dd_to = true;
					}
				} else dd = rr;
				var cell = dd;
				if(have_dd_to && this.allow_update) {
					if(this.active_column != null) this.active_column[j] = 1;
					if(this.sep == "") {
						if(this.builder.needSeparator()) {
							this.sep = this.getSeparator(this.a,this.b,"->");
							this.builder.setSeparator(this.sep);
						} else this.sep = "->";
					}
					var is_conflict = false;
					if(have_dd_to_alt) {
						if(!this.isEqual(this.v,dd_to,dd_to_alt)) is_conflict = true;
					}
					if(!is_conflict) {
						cell = this.builder.update(dd,dd_to);
						if(this.sep.length > this.act.length) this.act = this.sep;
					} else {
						if(this.conflict_sep == "") {
							if(this.builder.needSeparator()) {
								this.conflict_sep = this.getSeparator(this.p,this.a,"!") + this.sep;
								this.builder.setConflictSeparator(this.conflict_sep);
							} else this.conflict_sep = "!->";
						}
						cell = this.builder.conflict(dd,dd_to_alt,dd_to);
						this.act = this.conflict_sep;
					}
				}
				if(this.act == "" && this.have_addition) this.act = "+";
				if(this.act == "+++") {
					if(have_rr) {
						if(this.active_column != null) this.active_column[j] = 1;
					}
				}
				if(this.publish) {
					if(this.active_column == null || this.active_column[j] > 0) output.setCell(j + 1,at,cell);
				}
			}
			if(this.publish) {
				output.setCell(0,at,this.builder.marker(this.act));
				this.row_map.h[at] = unit;
			}
			if(this.act != "") {
				this.diff_found = true;
				if(!this.publish) {
					if(this.active_row != null) this.active_row[i] = 1;
				}
			}
		}
		,hilite: function(output) {
			output = coopy.Coopy.tablify(output);
			return this.hiliteSingle(output);
		}
		,hiliteSingle: function(output) {
			if(!output.isResizable()) return false;
			if(this.builder == null) {
				if(this.flags.allow_nested_cells) this.builder = new coopy.NestedCellBuilder(); else this.builder = new coopy.FlatCellBuilder(this.flags);
			}
			output.resize(0,0);
			output.clear();
			this.reset();
			this.setupTables();
			this.setupColumns();
			this.setupMoves();
			this.scanActivity();
			this.scanSchema();
			this.addSchema(output);
			this.addHeader(output);
			this.addMeta(output);
			var outer_reps_needed;
			if(this.flags.show_unchanged && this.flags.show_unchanged_columns) outer_reps_needed = 1; else outer_reps_needed = 2;
			var output_height = output.get_height();
			var output_height_init = output.get_height();
			var _g = 0;
			while(_g < outer_reps_needed) {
				var out = _g++;
				if(out == 1) {
					this.refineActivity();
					var rows = this.countActive(this.active_row) + output_height_init;
					if(this.top_line_done) rows--;
					output_height = output_height_init;
					if(rows > output.get_height()) output.resize(this.column_units.length + 1,rows);
				}
				var showed_dummy = false;
				var l = -1;
				var r = -1;
				var _g2 = 0;
				var _g1 = this.row_units.length;
				while(_g2 < _g1) {
					var i = _g2++;
					var unit = this.row_units[i];
					var reordered = false;
					if(this.flags.ordered) {
						if(this.row_moves.h.hasOwnProperty(i)) reordered = true;
						if(reordered) this.show_rc_numbers = true;
					}
					if(unit.r < 0 && unit.l < 0) continue;
					if(unit.r == 0 && unit.lp() <= 0 && this.top_line_done) continue;
					this.publish = this.flags.show_unchanged;
					var dummy = false;
					if(out == 1) {
						this.publish = this.active_row[i] > 0;
						dummy = this.active_row[i] == 3;
						if(dummy && showed_dummy) continue;
						if(!this.publish) continue;
					}
					if(!dummy) showed_dummy = false;
					var at = output_height;
					if(this.publish) {
						output_height++;
						if(output.get_height() < output_height) output.resize(this.column_units.length + 1,output_height);
					}
					if(dummy) {
						var _g4 = 0;
						var _g3 = this.column_units.length + 1;
						while(_g4 < _g3) {
							var j = _g4++;
							output.setCell(j,at,this.v.toDatum("..."));
						}
						showed_dummy = true;
						continue;
					}
					this.have_addition = false;
					var skip = false;
					this.act = "";
					if(reordered) this.act = ":";
					if(unit.p < 0 && unit.l < 0 && unit.r >= 0) {
						if(!this.allow_insert) skip = true;
						this.act = "+++";
					}
					if((unit.p >= 0 || !this.has_parent) && unit.l >= 0 && unit.r < 0) {
						if(!this.allow_delete) skip = true;
						this.act = "---";
					}
					if(skip) {
						if(!this.publish) {
							if(this.active_row != null) this.active_row[i] = -3;
						}
						continue;
					}
					this.scanRow(unit,output,at,i);
				}
			}
			this.checkRcNumbers(output.get_width(),output.get_height());
			var admin_w = this.addRcNumbers(output);
			if(!this.preserve_columns) this.elideColumns(output,admin_w);
			return true;
		}
		,hiliteWithNesting: function(output) {
			var base = output.add("base");
			var result = this.hiliteSingle(base);
			if(!result) return false;
			if(this.align.comp == null) return true;
			var order = this.align.comp.child_order;
			if(order == null) return true;
			output.alignment = this.align;
			var _g = 0;
			while(_g < order.length) {
				var name = order[_g];
				++_g;
				var child = this.align.comp.children.get(name);
				var alignment = child.alignment;
				if(alignment.isMarkedAsIdentical()) {
					this.align.comp.children.set(name,null);
					continue;
				}
				var td = new coopy.TableDiff(alignment,this.flags);
				var child_output = output.add(name);
				result = result && td.hiliteSingle(child_output);
			}
			return result;
		}
		,hasDifference: function() {
			return this.diff_found;
		}
		,hasSchemaDifference: function() {
			return this.schema_diff_found;
		}
		,isNested: function() {
			return this.nesting_present;
		}
		,getComparisonState: function() {
			if(this.align == null) return null;
			return this.align.comp;
		}
		,__class__: coopy.TableDiff
	};
	coopy.TableIO = $hx_exports.coopy.TableIO = function() {
	};
	coopy.TableIO.__name__ = true;
	coopy.TableIO.prototype = {
		valid: function() {
			return false;
		}
		,getContent: function(name) {
			return "";
		}
		,saveContent: function(name,txt) {
			return false;
		}
		,args: function() {
			return [];
		}
		,writeStdout: function(txt) {
		}
		,writeStderr: function(txt) {
		}
		,command: function(cmd,args) {
			return 1;
		}
		,async: function() {
			return false;
		}
		,exists: function(path) {
			return false;
		}
		,isTtyKnown: function() {
			return false;
		}
		,isTty: function() {
			return true;
		}
		,openSqliteDatabase: function(path) {
			return null;
		}
		,sendToBrowser: function(html) {
			console.log("do not know how to send to browser in this language");
		}
		,__class__: coopy.TableIO
	};
	coopy.TableModifier = $hx_exports.coopy.TableModifier = function(t) {
		this.t = t;
	};
	coopy.TableModifier.__name__ = true;
	coopy.TableModifier.prototype = {
		removeColumn: function(at) {
			var fate = [];
			var _g1 = 0;
			var _g = this.t.get_width();
			while(_g1 < _g) {
				var i = _g1++;
				if(i < at) fate.push(i); else if(i > at) fate.push(i - 1); else fate.push(-1);
			}
			return this.t.insertOrDeleteColumns(fate,this.t.get_width() - 1);
		}
		,__class__: coopy.TableModifier
	};
	coopy.TableStream = function(t) {
		this.t = t;
		this.at = -1;
		this.h = t.get_height();
		this.src = null;
		if(this.h < 0) {
			var meta = t.getMeta();
			if(meta == null) throw "Cannot get meta information for table";
			this.src = meta.getRowStream();
			if(this.src == null) throw "Cannot iterate table";
		}
	};
	coopy.TableStream.__name__ = true;
	coopy.TableStream.__interfaces__ = [coopy.RowStream];
	coopy.TableStream.prototype = {
		fetchColumns: function() {
			if(this.columns != null) return this.columns;
			if(this.src != null) {
				this.columns = this.src.fetchColumns();
				return this.columns;
			}
			this.columns = [];
			var _g1 = 0;
			var _g = this.t.get_width();
			while(_g1 < _g) {
				var i = _g1++;
				this.columns.push(this.t.getCell(i,0));
			}
			return this.columns;
		}
		,fetchRow: function() {
			if(this.src != null) return this.src.fetchRow();
			if(this.at >= this.h) return null;
			var row = new haxe.ds.StringMap();
			var _g1 = 0;
			var _g = this.columns.length;
			while(_g1 < _g) {
				var i = _g1++;
				var v = this.t.getCell(i,this.at);
				row.set(this.columns[i],v);
				v;
			}
			return row;
		}
		,fetch: function() {
			if(this.at == -1) {
				this.at++;
				if(this.src != null) this.fetchColumns();
				return true;
			}
			if(this.src != null) {
				this.at = 1;
				this.row = this.fetchRow();
				return this.row != null;
			}
			this.at++;
			return this.at < this.h;
		}
		,getCell: function(x) {
			if(this.at == 0) return this.columns[x];
			if(this.row != null) return this.row.get(this.columns[x]);
			return this.t.getCell(x,this.at);
		}
		,width: function() {
			this.fetchColumns();
			return this.columns.length;
		}
		,__class__: coopy.TableStream
	};
	coopy.Tables = $hx_exports.coopy.Tables = function(template) {
		this.template = template;
		this.tables = new haxe.ds.StringMap();
		this.table_order = [];
	};
	coopy.Tables.__name__ = true;
	coopy.Tables.prototype = {
		add: function(name) {
			var t = this.template.clone();
			this.tables.set(name,t);
			this.table_order.push(name);
			return t;
		}
		,getOrder: function() {
			return this.table_order;
		}
		,get: function(name) {
			return this.tables.get(name);
		}
		,one: function() {
			return this.tables.get(this.table_order[0]);
		}
		,hasInsDel: function() {
			if(this.alignment == null) return false;
			if(this.alignment.has_addition) return true;
			if(this.alignment.has_removal) return true;
			return false;
		}
		,__class__: coopy.Tables
	};
	coopy.TerminalDiffRender = $hx_exports.coopy.TerminalDiffRender = function(flags) {
		this.align_columns = true;
		this.wide_columns = false;
		this.flags = flags;
		if(flags != null) {
			if(flags.padding_strategy == "dense") this.align_columns = false;
			if(flags.padding_strategy == "sparse") this.wide_columns = true;
		}
	};
	coopy.TerminalDiffRender.__name__ = true;
	coopy.TerminalDiffRender.prototype = {
		alignColumns: function(enable) {
			this.align_columns = enable;
		}
		,render: function(t) {
			this.csv = new coopy.Csv();
			var result = "";
			var w = t.get_width();
			var h = t.get_height();
			var txt = "";
			this.t = t;
			this.v = t.getCellView();
			this.codes = new haxe.ds.StringMap();
			this.codes.set("header","\x1B[0;1m");
			this.codes.set("meta","\x1B[0;1m");
			this.codes.set("spec","\x1B[35;1m");
			this.codes.set("add","\x1B[32;1m");
			this.codes.set("conflict","\x1B[33;1m");
			this.codes.set("modify","\x1B[34;1m");
			this.codes.set("remove","\x1B[31;1m");
			this.codes.set("minor","\x1B[2m");
			this.codes.set("done","\x1B[0m");
			var sizes = null;
			if(this.align_columns) sizes = this.pickSizes(t);
			var _g = 0;
			while(_g < h) {
				var y = _g++;
				var target = 0;
				var at = 0;
				var _g1 = 0;
				while(_g1 < w) {
					var x = _g1++;
					if(x > 0) txt += this.codes.get("minor") + "," + this.codes.get("done");
					if(sizes != null) {
						var spaces = target - at;
						var _g2 = 0;
						while(_g2 < spaces) {
							var i = _g2++;
							txt += " ";
							at++;
						}
					}
					txt += this.getText(x,y,true);
					if(sizes != null) {
						var bit = this.getText(x,y,false);
						at += bit.length;
						target += sizes[x];
					}
				}
				txt += "\r\n";
			}
			this.t = null;
			this.v = null;
			this.csv = null;
			this.codes = null;
			return txt;
		}
		,getText: function(x,y,color) {
			var val = this.t.getCell(x,y);
			var cell = coopy.DiffRender.renderCell(this.t,this.v,x,y);
			if(color) {
				var code = null;
				if(cell.category != null) code = this.codes.get(cell.category);
				if(cell.category_given_tr != null) {
					var code_tr = this.codes.get(cell.category_given_tr);
					if(code_tr != null) code = code_tr;
				}
				if(code != null) {
					if(cell.rvalue != null) {
						val = this.codes.get("remove") + cell.lvalue + this.codes.get("modify") + cell.pretty_separator + this.codes.get("add") + cell.rvalue + this.codes.get("done");
						if(cell.pvalue != null) val = this.codes.get("conflict") + cell.pvalue + this.codes.get("modify") + cell.pretty_separator + Std.string(val);
					} else {
						val = cell.pretty_value;
						val = code + Std.string(val) + this.codes.get("done");
					}
				}
			} else val = cell.pretty_value;
			return this.csv.renderCell(this.v,val);
		}
		,pickSizes: function(t) {
			var w = t.get_width();
			var h = t.get_height();
			var v = t.getCellView();
			var csv = new coopy.Csv();
			var sizes = [];
			var row = -1;
			var total = w - 1;
			var _g = 0;
			while(_g < w) {
				var x = _g++;
				var m = 0;
				var m2 = 0;
				var mmax = 0;
				var mmostmax = 0;
				var mmin = -1;
				var _g1 = 0;
				while(_g1 < h) {
					var y = _g1++;
					var txt = this.getText(x,y,false);
					if(txt == "@@" && row == -1) row = y;
					var len = txt.length;
					if(y == row) mmin = len;
					m += len;
					m2 += len * len;
					if(len > mmax) mmax = len;
				}
				var mean = m / h;
				var stddev = Math.sqrt(m2 / h - mean * mean);
				var most = mean + stddev * 2 + 0.5 | 0;
				var _g11 = 0;
				while(_g11 < h) {
					var y1 = _g11++;
					var txt1 = this.getText(x,y1,false);
					var len1 = txt1.length;
					if(len1 <= most) {
						if(len1 > mmostmax) mmostmax = len1;
					}
				}
				var full = mmax;
				most = mmostmax;
				if(mmin != -1) {
					if(most < mmin) most = mmin;
				}
				if(this.wide_columns) most = full;
				sizes.push(most);
				total += most;
			}
			if(total > 130) return null;
			return sizes;
		}
		,__class__: coopy.TerminalDiffRender
	};
	coopy.Unit = function(l,r,p) {
		if(p == null) p = -2;
		if(r == null) r = -2;
		if(l == null) l = -2;
		this.l = l;
		this.r = r;
		this.p = p;
	};
	coopy.Unit.__name__ = true;
	coopy.Unit.describe = function(i) {
		if(i >= 0) return "" + i; else return "-";
	};
	coopy.Unit.prototype = {
		lp: function() {
			if(this.p == -2) return this.l; else return this.p;
		}
		,toString: function() {
			if(this.p >= -1) return coopy.Unit.describe(this.p) + "|" + coopy.Unit.describe(this.l) + ":" + coopy.Unit.describe(this.r);
			return coopy.Unit.describe(this.l) + ":" + coopy.Unit.describe(this.r);
		}
		,fromString: function(txt) {
			txt += "]";
			var at = 0;
			var _g1 = 0;
			var _g = txt.length;
			while(_g1 < _g) {
				var i = _g1++;
				var ch = HxOverrides.cca(txt,i);
				if(ch >= 48 && ch <= 57) {
					at *= 10;
					at += ch - 48;
				} else if(ch == 45) at = -1; else if(ch == 124) {
					this.p = at;
					at = 0;
				} else if(ch == 58) {
					this.l = at;
					at = 0;
				} else if(ch == 93) {
					this.r = at;
					return true;
				}
			}
			return false;
		}
		,base26: function(num) {
			var alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
			if(num < 0) return "-";
			var out = "";
			do {
				out = out + alpha.charAt(num % 26);
				num = Math.floor(num / 26) - 1;
			} while(num >= 0);
			return out;
		}
		,toBase26String: function() {
			if(this.p >= -1) return this.base26(this.p) + "|" + this.base26(this.l) + ":" + this.base26(this.r);
			return this.base26(this.l) + ":" + this.base26(this.r);
		}
		,__class__: coopy.Unit
	};
	coopy.Viterbi = $hx_exports.coopy.Viterbi = function() {
		this.K = this.T = 0;
		this.reset();
		this.cost = new coopy.SparseSheet();
		this.src = new coopy.SparseSheet();
		this.path = new coopy.SparseSheet();
	};
	coopy.Viterbi.__name__ = true;
	coopy.Viterbi.prototype = {
		reset: function() {
			this.index = 0;
			this.mode = 0;
			this.path_valid = false;
			this.best_cost = 0;
		}
		,setSize: function(states,sequence_length) {
			this.K = states;
			this.T = sequence_length;
			this.cost.resize(this.K,this.T,0);
			this.src.resize(this.K,this.T,-1);
			this.path.resize(1,this.T,-1);
		}
		,assertMode: function(next) {
			if(next == 0 && this.mode == 1) this.index++;
			this.mode = next;
		}
		,addTransition: function(s0,s1,c) {
			var resize = false;
			if(s0 >= this.K) {
				this.K = s0 + 1;
				resize = true;
			}
			if(s1 >= this.K) {
				this.K = s1 + 1;
				resize = true;
			}
			if(resize) {
				this.cost.nonDestructiveResize(this.K,this.T,0);
				this.src.nonDestructiveResize(this.K,this.T,-1);
				this.path.nonDestructiveResize(1,this.T,-1);
			}
			this.path_valid = false;
			this.assertMode(1);
			if(this.index >= this.T) {
				this.T = this.index + 1;
				this.cost.nonDestructiveResize(this.K,this.T,0);
				this.src.nonDestructiveResize(this.K,this.T,-1);
				this.path.nonDestructiveResize(1,this.T,-1);
			}
			var sourced = false;
			if(this.index > 0) {
				c += this.cost.get(s0,this.index - 1);
				sourced = this.src.get(s0,this.index - 1) != -1;
			} else sourced = true;
			if(sourced) {
				if(c < this.cost.get(s1,this.index) || this.src.get(s1,this.index) == -1) {
					this.cost.set(s1,this.index,c);
					this.src.set(s1,this.index,s0);
				}
			}
		}
		,endTransitions: function() {
			this.path_valid = false;
			this.assertMode(0);
		}
		,beginTransitions: function() {
			this.path_valid = false;
			this.assertMode(1);
		}
		,calculatePath: function() {
			if(this.path_valid) return;
			this.endTransitions();
			var best = 0;
			var bestj = -1;
			if(this.index <= 0) {
				this.path_valid = true;
				return;
			}
			var _g1 = 0;
			var _g = this.K;
			while(_g1 < _g) {
				var j = _g1++;
				if((this.cost.get(j,this.index - 1) < best || bestj == -1) && this.src.get(j,this.index - 1) != -1) {
					best = this.cost.get(j,this.index - 1);
					bestj = j;
				}
			}
			this.best_cost = best;
			var _g11 = 0;
			var _g2 = this.index;
			while(_g11 < _g2) {
				var j1 = _g11++;
				var i = this.index - 1 - j1;
				this.path.set(0,i,bestj);
				if(!(bestj != -1 && (bestj >= 0 && bestj < this.K))) console.log("Problem in Viterbi");
				bestj = this.src.get(bestj,i);
			}
			this.path_valid = true;
		}
		,toString: function() {
			this.calculatePath();
			var txt = "";
			var _g1 = 0;
			var _g = this.index;
			while(_g1 < _g) {
				var i = _g1++;
				if(this.path.get(0,i) == -1) txt += "*"; else txt += this.path.get(0,i);
				if(this.K >= 10) txt += " ";
			}
			txt += " costs " + this.getCost();
			return txt;
		}
		,length: function() {
			if(this.index > 0) this.calculatePath();
			return this.index;
		}
		,get: function(i) {
			this.calculatePath();
			return this.path.get(0,i);
		}
		,getCost: function() {
			this.calculatePath();
			return this.best_cost;
		}
		,__class__: coopy.Viterbi
	};
	var haxe = {};
	haxe.IMap = function() { };
	haxe.IMap.__name__ = true;
	haxe.IMap.prototype = {
		__class__: haxe.IMap
	};
	haxe.Json = function() { };
	haxe.Json.__name__ = true;
	haxe.Json.stringify = function(value,replacer,space) {
		return haxe.format.JsonPrinter.print(value,replacer,space);
	};
	haxe.ds = {};
	haxe.ds.IntMap = function() {
		this.h = { };
	};
	haxe.ds.IntMap.__name__ = true;
	haxe.ds.IntMap.__interfaces__ = [haxe.IMap];
	haxe.ds.IntMap.prototype = {
		set: function(key,value) {
			this.h[key] = value;
		}
		,get: function(key) {
			return this.h[key];
		}
		,remove: function(key) {
			if(!this.h.hasOwnProperty(key)) return false;
			delete(this.h[key]);
			return true;
		}
		,keys: function() {
			var a = [];
			for( var key in this.h ) {
			if(this.h.hasOwnProperty(key)) a.push(key | 0);
			}
			return HxOverrides.iter(a);
		}
		,toString: function() {
			var s_b = "";
			s_b += "{";
			var it = this.keys();
			while( it.hasNext() ) {
				var i = it.next();
				if(i == null) s_b += "null"; else s_b += "" + i;
				s_b += " => ";
				s_b += Std.string(Std.string(this.h[i]));
				if(it.hasNext()) s_b += ", ";
			}
			s_b += "}";
			return s_b;
		}
		,__class__: haxe.ds.IntMap
	};
	haxe.ds._StringMap = {};
	haxe.ds._StringMap.StringMapIterator = function(map,keys) {
		this.map = map;
		this.keys = keys;
		this.index = 0;
		this.count = keys.length;
	};
	haxe.ds._StringMap.StringMapIterator.__name__ = true;
	haxe.ds._StringMap.StringMapIterator.prototype = {
		hasNext: function() {
			return this.index < this.count;
		}
		,next: function() {
			return this.map.get(this.keys[this.index++]);
		}
		,__class__: haxe.ds._StringMap.StringMapIterator
	};
	haxe.ds.StringMap = function() {
		this.h = { };
	};
	haxe.ds.StringMap.__name__ = true;
	haxe.ds.StringMap.__interfaces__ = [haxe.IMap];
	haxe.ds.StringMap.prototype = {
		set: function(key,value) {
			if(__map_reserved[key] != null) this.setReserved(key,value); else this.h[key] = value;
		}
		,get: function(key) {
			if(__map_reserved[key] != null) return this.getReserved(key);
			return this.h[key];
		}
		,exists: function(key) {
			if(__map_reserved[key] != null) return this.existsReserved(key);
			return this.h.hasOwnProperty(key);
		}
		,setReserved: function(key,value) {
			if(this.rh == null) this.rh = { };
			this.rh["$" + key] = value;
		}
		,getReserved: function(key) {
			if(this.rh == null) return null; else return this.rh["$" + key];
		}
		,existsReserved: function(key) {
			if(this.rh == null) return false;
			return this.rh.hasOwnProperty("$" + key);
		}
		,keys: function() {
			var _this = this.arrayKeys();
			return HxOverrides.iter(_this);
		}
		,arrayKeys: function() {
			var out = [];
			for( var key in this.h ) {
			if(this.h.hasOwnProperty(key)) out.push(key);
			}
			if(this.rh != null) {
				for( var key in this.rh ) {
				if(key.charCodeAt(0) == 36) out.push(key.substr(1));
				}
			}
			return out;
		}
		,iterator: function() {
			return new haxe.ds._StringMap.StringMapIterator(this,this.arrayKeys());
		}
		,__class__: haxe.ds.StringMap
	};
	haxe.format = {};
	haxe.format.JsonParser = function(str) {
		this.str = str;
		this.pos = 0;
	};
	haxe.format.JsonParser.__name__ = true;
	haxe.format.JsonParser.prototype = {
		parseRec: function() {
			while(true) {
				var c = StringTools.fastCodeAt(this.str,this.pos++);
				switch(c) {
				case 32:case 13:case 10:case 9:
					break;
				case 123:
					var obj = { };
					var field = null;
					var comma = null;
					while(true) {
						var c1 = StringTools.fastCodeAt(this.str,this.pos++);
						switch(c1) {
						case 32:case 13:case 10:case 9:
							break;
						case 125:
							if(field != null || comma == false) this.invalidChar();
							return obj;
						case 58:
							if(field == null) this.invalidChar();
							Reflect.setField(obj,field,this.parseRec());
							field = null;
							comma = true;
							break;
						case 44:
							if(comma) comma = false; else this.invalidChar();
							break;
						case 34:
							if(comma) this.invalidChar();
							field = this.parseString();
							break;
						default:
							this.invalidChar();
						}
					}
					break;
				case 91:
					var arr = [];
					var comma1 = null;
					while(true) {
						var c2 = StringTools.fastCodeAt(this.str,this.pos++);
						switch(c2) {
						case 32:case 13:case 10:case 9:
							break;
						case 93:
							if(comma1 == false) this.invalidChar();
							return arr;
						case 44:
							if(comma1) comma1 = false; else this.invalidChar();
							break;
						default:
							if(comma1) this.invalidChar();
							this.pos--;
							arr.push(this.parseRec());
							comma1 = true;
						}
					}
					break;
				case 116:
					var save = this.pos;
					if(StringTools.fastCodeAt(this.str,this.pos++) != 114 || StringTools.fastCodeAt(this.str,this.pos++) != 117 || StringTools.fastCodeAt(this.str,this.pos++) != 101) {
						this.pos = save;
						this.invalidChar();
					}
					return true;
				case 102:
					var save1 = this.pos;
					if(StringTools.fastCodeAt(this.str,this.pos++) != 97 || StringTools.fastCodeAt(this.str,this.pos++) != 108 || StringTools.fastCodeAt(this.str,this.pos++) != 115 || StringTools.fastCodeAt(this.str,this.pos++) != 101) {
						this.pos = save1;
						this.invalidChar();
					}
					return false;
				case 110:
					var save2 = this.pos;
					if(StringTools.fastCodeAt(this.str,this.pos++) != 117 || StringTools.fastCodeAt(this.str,this.pos++) != 108 || StringTools.fastCodeAt(this.str,this.pos++) != 108) {
						this.pos = save2;
						this.invalidChar();
					}
					return null;
				case 34:
					return this.parseString();
				case 48:case 49:case 50:case 51:case 52:case 53:case 54:case 55:case 56:case 57:case 45:
					return this.parseNumber(c);
				default:
					this.invalidChar();
				}
			}
		}
		,parseString: function() {
			var start = this.pos;
			var buf_b = "";
			while(true) {
				var c = StringTools.fastCodeAt(this.str,this.pos++);
				if(c == 34) break;
				if(c == 92) {
					buf_b += HxOverrides.substr(this.str,start,this.pos - start - 1);
					c = StringTools.fastCodeAt(this.str,this.pos++);
					switch(c) {
					case 114:
						buf_b += "\r";
						break;
					case 110:
						buf_b += "\n";
						break;
					case 116:
						buf_b += "\t";
						break;
					case 98:
						buf_b += "\x08";
						break;
					case 102:
						buf_b += "\x0C";
						break;
					case 47:case 92:case 34:
						buf_b += String.fromCharCode(c);
						break;
					case 117:
						var uc = Std.parseInt("0x" + HxOverrides.substr(this.str,this.pos,4));
						this.pos += 4;
						buf_b += String.fromCharCode(uc);
						break;
					default:
						throw "Invalid escape sequence \\" + String.fromCharCode(c) + " at position " + (this.pos - 1);
					}
					start = this.pos;
				} else if(c != c) throw "Unclosed string";
			}
			buf_b += HxOverrides.substr(this.str,start,this.pos - start - 1);
			return buf_b;
		}
		,parseNumber: function(c) {
			var start = this.pos - 1;
			var minus = c == 45;
			var digit = !minus;
			var zero = c == 48;
			var point = false;
			var e = false;
			var pm = false;
			var end = false;
			while(true) {
				c = StringTools.fastCodeAt(this.str,this.pos++);
				switch(c) {
				case 48:
					if(zero && !point) this.invalidNumber(start);
					if(minus) {
						minus = false;
						zero = true;
					}
					digit = true;
					break;
				case 49:case 50:case 51:case 52:case 53:case 54:case 55:case 56:case 57:
					if(zero && !point) this.invalidNumber(start);
					if(minus) minus = false;
					digit = true;
					zero = false;
					break;
				case 46:
					if(minus || point) this.invalidNumber(start);
					digit = false;
					point = true;
					break;
				case 101:case 69:
					if(minus || zero || e) this.invalidNumber(start);
					digit = false;
					e = true;
					break;
				case 43:case 45:
					if(!e || pm) this.invalidNumber(start);
					digit = false;
					pm = true;
					break;
				default:
					if(!digit) this.invalidNumber(start);
					this.pos--;
					end = true;
				}
				if(end) break;
			}
			var f = Std.parseFloat(HxOverrides.substr(this.str,start,this.pos - start));
			var i = f | 0;
			if(i == f) return i; else return f;
		}
		,invalidChar: function() {
			this.pos--;
			throw "Invalid char " + this.str.charCodeAt(this.pos) + " at position " + this.pos;
		}
		,invalidNumber: function(start) {
			throw "Invalid number at position " + start + ": " + HxOverrides.substr(this.str,start,this.pos - start);
		}
		,__class__: haxe.format.JsonParser
	};
	haxe.format.JsonPrinter = function(replacer,space) {
		this.replacer = replacer;
		this.indent = space;
		this.pretty = space != null;
		this.nind = 0;
		this.buf = new StringBuf();
	};
	haxe.format.JsonPrinter.__name__ = true;
	haxe.format.JsonPrinter.print = function(o,replacer,space) {
		var printer = new haxe.format.JsonPrinter(replacer,space);
		printer.write("",o);
		return printer.buf.b;
	};
	haxe.format.JsonPrinter.prototype = {
		ipad: function() {
			if(this.pretty) {
				var v = StringTools.lpad("",this.indent,this.nind * this.indent.length);
				if(v == null) this.buf.b += "null"; else this.buf.b += "" + v;
			}
		}
		,write: function(k,v) {
			if(this.replacer != null) v = this.replacer(k,v);
			{
				var _g = Type["typeof"](v);
				switch(_g[1]) {
				case 8:
					this.buf.b += "\"???\"";
					break;
				case 4:
					this.fieldsString(v,Reflect.fields(v));
					break;
				case 1:
					var v1 = v;
					if(v1 == null) this.buf.b += "null"; else this.buf.b += "" + v1;
					break;
				case 2:
					var v2;
					if((function($this) {
						var $r;
						var f = v;
						$r = isFinite(f);
						return $r;
					}(this))) v2 = v; else v2 = "null";
					if(v2 == null) this.buf.b += "null"; else this.buf.b += "" + v2;
					break;
				case 5:
					this.buf.b += "\"<fun>\"";
					break;
				case 6:
					var c = _g[2];
					if(c == String) this.quote(v); else if(c == Array) {
						var v3 = v;
						this.buf.b += "[";
						var len = v3.length;
						var last = len - 1;
						var _g1 = 0;
						while(_g1 < len) {
							var i = _g1++;
							if(i > 0) this.buf.b += ","; else this.nind++;
							if(this.pretty) this.buf.b += "\n";
							this.ipad();
							this.write(i,v3[i]);
							if(i == last) {
								this.nind--;
								if(this.pretty) this.buf.b += "\n";
								this.ipad();
							}
						}
						this.buf.b += "]";
					} else if(c == haxe.ds.StringMap) {
						var v4 = v;
						var o = { };
						var $it0 = v4.keys();
						while( $it0.hasNext() ) {
							var k1 = $it0.next();
							Reflect.setField(o,k1,__map_reserved[k1] != null?v4.getReserved(k1):v4.h[k1]);
						}
						this.fieldsString(o,Reflect.fields(o));
					} else if(c == Date) {
						var v5 = v;
						this.quote(HxOverrides.dateStr(v5));
					} else this.fieldsString(v,Reflect.fields(v));
					break;
				case 7:
					var i1 = Type.enumIndex(v);
					var v6 = i1;
					if(v6 == null) this.buf.b += "null"; else this.buf.b += "" + v6;
					break;
				case 3:
					var v7 = v;
					if(v7 == null) this.buf.b += "null"; else this.buf.b += "" + v7;
					break;
				case 0:
					this.buf.b += "null";
					break;
				}
			}
		}
		,fieldsString: function(v,fields) {
			this.buf.b += "{";
			var len = fields.length;
			var last = len - 1;
			var first = true;
			var _g = 0;
			while(_g < len) {
				var i = _g++;
				var f = fields[i];
				var value = Reflect.field(v,f);
				if(Reflect.isFunction(value)) continue;
				if(first) {
					this.nind++;
					first = false;
				} else this.buf.b += ",";
				if(this.pretty) this.buf.b += "\n";
				this.ipad();
				this.quote(f);
				this.buf.b += ":";
				if(this.pretty) this.buf.b += " ";
				this.write(f,value);
				if(i == last) {
					this.nind--;
					if(this.pretty) this.buf.b += "\n";
					this.ipad();
				}
			}
			this.buf.b += "}";
		}
		,quote: function(s) {
			this.buf.b += "\"";
			var i = 0;
			while(true) {
				var c = StringTools.fastCodeAt(s,i++);
				if(c != c) break;
				switch(c) {
				case 34:
					this.buf.b += "\\\"";
					break;
				case 92:
					this.buf.b += "\\\\";
					break;
				case 10:
					this.buf.b += "\\n";
					break;
				case 13:
					this.buf.b += "\\r";
					break;
				case 9:
					this.buf.b += "\\t";
					break;
				case 8:
					this.buf.b += "\\b";
					break;
				case 12:
					this.buf.b += "\\f";
					break;
				default:
					this.buf.b += String.fromCharCode(c);
				}
			}
			this.buf.b += "\"";
		}
		,__class__: haxe.format.JsonPrinter
	};
	var js = {};
	js.Boot = function() { };
	js.Boot.__name__ = true;
	js.Boot.getClass = function(o) {
		if((o instanceof Array) && o.__enum__ == null) return Array; else {
			var cl = o.__class__;
			if(cl != null) return cl;
			var name = js.Boot.__nativeClassName(o);
			if(name != null) return js.Boot.__resolveNativeClass(name);
			return null;
		}
	};
	js.Boot.__string_rec = function(o,s) {
		if(o == null) return "null";
		if(s.length >= 5) return "<...>";
		var t = typeof(o);
		if(t == "function" && (o.__name__ || o.__ename__)) t = "object";
		switch(t) {
		case "object":
			if(o instanceof Array) {
				if(o.__enum__) {
					if(o.length == 2) return o[0];
					var str2 = o[0] + "(";
					s += "\t";
					var _g1 = 2;
					var _g = o.length;
					while(_g1 < _g) {
						var i1 = _g1++;
						if(i1 != 2) str2 += "," + js.Boot.__string_rec(o[i1],s); else str2 += js.Boot.__string_rec(o[i1],s);
					}
					return str2 + ")";
				}
				var l = o.length;
				var i;
				var str1 = "[";
				s += "\t";
				var _g2 = 0;
				while(_g2 < l) {
					var i2 = _g2++;
					str1 += (i2 > 0?",":"") + js.Boot.__string_rec(o[i2],s);
				}
				str1 += "]";
				return str1;
			}
			var tostr;
			try {
				tostr = o.toString;
			} catch( e ) {
				return "???";
			}
			if(tostr != null && tostr != Object.toString && typeof(tostr) == "function") {
				var s2 = o.toString();
				if(s2 != "[object Object]") return s2;
			}
			var k = null;
			var str = "{\n";
			s += "\t";
			var hasp = o.hasOwnProperty != null;
			for( var k in o ) {
			if(hasp && !o.hasOwnProperty(k)) {
				continue;
			}
			if(k == "prototype" || k == "__class__" || k == "__super__" || k == "__interfaces__" || k == "__properties__") {
				continue;
			}
			if(str.length != 2) str += ", \n";
			str += s + k + " : " + js.Boot.__string_rec(o[k],s);
			}
			s = s.substring(1);
			str += "\n" + s + "}";
			return str;
		case "function":
			return "<function>";
		case "string":
			return o;
		default:
			return String(o);
		}
	};
	js.Boot.__interfLoop = function(cc,cl) {
		if(cc == null) return false;
		if(cc == cl) return true;
		var intf = cc.__interfaces__;
		if(intf != null) {
			var _g1 = 0;
			var _g = intf.length;
			while(_g1 < _g) {
				var i = _g1++;
				var i1 = intf[i];
				if(i1 == cl || js.Boot.__interfLoop(i1,cl)) return true;
			}
		}
		return js.Boot.__interfLoop(cc.__super__,cl);
	};
	js.Boot.__instanceof = function(o,cl) {
		if(cl == null) return false;
		switch(cl) {
		case Int:
			return (o|0) === o;
		case Float:
			return typeof(o) == "number";
		case Bool:
			return typeof(o) == "boolean";
		case String:
			return typeof(o) == "string";
		case Array:
			return (o instanceof Array) && o.__enum__ == null;
		case Dynamic:
			return true;
		default:
			if(o != null) {
				if(typeof(cl) == "function") {
					if(o instanceof cl) return true;
					if(js.Boot.__interfLoop(js.Boot.getClass(o),cl)) return true;
				} else if(typeof(cl) == "object" && js.Boot.__isNativeObj(cl)) {
					if(o instanceof cl) return true;
				}
			} else return false;
			if(cl == Class && o.__name__ != null) return true;
			if(cl == Enum && o.__ename__ != null) return true;
			return o.__enum__ == cl;
		}
	};
	js.Boot.__nativeClassName = function(o) {
		var name = js.Boot.__toStr.call(o).slice(8,-1);
		if(name == "Object" || name == "Function" || name == "Math" || name == "JSON") return null;
		return name;
	};
	js.Boot.__isNativeObj = function(o) {
		return js.Boot.__nativeClassName(o) != null;
	};
	js.Boot.__resolveNativeClass = function(name) {
		if(typeof window != "undefined") return window[name]; else return global[name];
	};
	function $iterator(o) { if( o instanceof Array ) return function() { return HxOverrides.iter(o); }; return typeof(o.iterator) == 'function' ? $bind(o,o.iterator) : o.iterator; }
	var $_, $fid = 0;
	function $bind(o,m) { if( m == null ) return null; if( m.__id__ == null ) m.__id__ = $fid++; var f; if( o.hx__closures__ == null ) o.hx__closures__ = {}; else f = o.hx__closures__[m.__id__]; if( f == null ) { f = function(){ return f.method.apply(f.scope, arguments); }; f.scope = o; f.method = m; o.hx__closures__[m.__id__] = f; } return f; }
	String.prototype.__class__ = String;
	String.__name__ = true;
	Array.__name__ = true;
	Date.prototype.__class__ = Date;
	Date.__name__ = ["Date"];
	var Int = { __name__ : ["Int"]};
	var Dynamic = { __name__ : ["Dynamic"]};
	var Float = Number;
	Float.__name__ = ["Float"];
	var Bool = Boolean;
	Bool.__ename__ = ["Bool"];
	var Class = { __name__ : ["Class"]};
	var Enum = { };
	var __map_reserved = {}
	coopy.Coopy.VERSION = "1.3.16";
	js.Boot.__toStr = {}.toString;
	coopy.Coopy.main();
	})(typeof console != "undefined" ? console : {log:function(){}}, typeof window != "undefined" ? window : exports);


	if (typeof exports != "undefined") {
	    // avoid having excess nesting (coopy.coopy) when using node
	    for (f in exports.coopy) { 
		if (exports.coopy.hasOwnProperty(f)) {
		    exports[f] = exports.coopy[f]; 
		}
	    } 
	    // promote methods of coopy.Coopy
	    for (f in exports.Coopy) { 
		if (exports.Coopy.hasOwnProperty(f)) {
		    exports[f] = exports.Coopy[f]; 
		}
	    } 
	} else {
	    // promote methods of coopy.Coopy
	    for (f in coopy.Coopy) { 
		if (coopy.Coopy.hasOwnProperty(f)) {
		    coopy[f] = coopy.Coopy[f]; 
		}
	    } 
	    daff = coopy;
	}
	(function() {

	var coopy = null;
	if (typeof exports != "undefined") {
	    if (typeof exports.Coopy != "undefined") {
		coopy = exports;
	    }
	}
	if (coopy == null) {
	    coopy = window.daff;
	}

	var CellView = function() {
	}

	CellView.prototype.toString = function(d) {
	    return ""+d;
	}

	CellView.prototype.equals = function(d1,d2) {
	    return d1==d2;
	}

	CellView.prototype.toDatum = function(d) {
	    return d;
	}

	CellView.prototype.makeHash = function() {
	    return {};
	}

	CellView.prototype.hashSet = function(d,k,v) {
	    d[k] = v;
	}

	CellView.prototype.hashGet = function(d,k) {
	    return d[k];
	}

	CellView.prototype.hashExists = function(d,k) {
	    return k in d;
	}

	CellView.prototype.isHash = function(d) {
	    return d && (typeof d  === "object");
	}

	var TableView = function(data) {
	    // variant constructor (cols, rows)
	    if (arguments.length==2) {
		var lst = [];
		for (var i=0; i<arguments[1]; i++) {
		    var row = [];
		    for (var j=0; j<arguments[0]; j++) {
			row.push(null);
		    }
		    lst.push(row);
		}
		data = lst;
	    }
	    this.data = data;
	    this.height = data.length;
	    this.width = 0;
	    if (this.height>0) {
		this.width = data[0].length;
	    }
	}

	TableView.prototype.get_width = function() {
	    return this.width;
	}

	TableView.prototype.get_height = function() {
	    return this.height;
	}

	TableView.prototype.getCell = function(x,y) {
	    return this.data[y][x];
	}

	TableView.prototype.setCell = function(x,y,c) {
	    this.data[y][x] = c;
	}

	TableView.prototype.toString = function() {
	    return coopy.SimpleTable.tableToString(this);
	}

	TableView.prototype.getCellView = function() {
	    return new CellView();
	    //return new coopy.SimpleView();
	}

	TableView.prototype.isResizable = function() {
	    return true;
	}

	TableView.prototype.resize = function(w,h) {
	    this.width = w;
	    this.height = h;
	    for (var i=0; i<this.data.length; i++) {
		var row = this.data[i];
		if (row==null) {
		    row = this.data[i] = [];
		}
		while (row.length<this.width) {
		    row.push(null);
		}
	    }
	    if (this.data.length<this.height) {
		while (this.data.length<this.height) {
		    var row = [];
		    for (var i=0; i<this.width; i++) {
			row.push(null);
		    }
		    this.data.push(row);
		}
	    }
	    return true;
	}

	TableView.prototype.clear = function() {
	    for (var i=0; i<this.data.length; i++) {
		var row = this.data[i];
		for (var j=0; j<row.length; j++) {
		    row[j] = null;
		}
	    }
	}

	TableView.prototype.trim = function() {
	    var changed = this.trimRows();
	    changed = changed || this.trimColumns();
	    return changed;
	}

	TableView.prototype.trimRows = function() {
	    var changed = false;
	    while (true) {
		if (this.height==0) return changed;
		var row = this.data[this.height-1];
		for (var i=0; i<this.width; i++) {
		    var c = row[i];
		    if (c!=null && c!="") return changed;
		}
		this.height--;
	    }
	}

	TableView.prototype.trimColumns = function() {
	    var top_content = 0;
	    for (var i=0; i<this.height; i++) {
		if (top_content>=this.width) break;
		var row = this.data[i];
		for (var j=0; j<this.width; j++) {
		    var c = row[j];
		    if (c!=null && c!="") {
			if (j>top_content) {
			    top_content = j;
			}
		    }
		}
	    }
	    if (this.height==0 || top_content+1==this.width) return false;
	    this.width = top_content+1;
	    return true;
	}

	TableView.prototype.getData = function() {
	    return this.data;
	}

	TableView.prototype.clone = function() {
	    var ndata = [];
	    for (var i=0; i<this.get_height(); i++) {
		ndata[i] = this.data[i].slice();
	    }
	    return new TableView(ndata);
	}

	TableView.prototype.create = function() {
	    return new TableView([]);
	}

	TableView.prototype.insertOrDeleteRows = function(fate, hfate) {
	    var ndata = [];
	    for (var i=0; i<fate.length; i++) {
	        var j = fate[i];
	        if (j!=-1) {
		    ndata[j] = this.data[i];
	        }
	    }
	    // let's preserve data
	    //this.data = ndata;
	    this.data.length = 0;
	    for (var i=0; i<ndata.length; i++) {
		this.data[i] = ndata[i];
	    }
	    this.resize(this.width,hfate);
	    return true;
	}

	TableView.prototype.insertOrDeleteColumns = function(fate, wfate) {
	    if (wfate==this.width && wfate==fate.length) {
		var eq = true;
		for (var i=0; i<wfate; i++) {
		    if (fate[i]!=i) {
			eq = false;
			break;
		    }
		}
		if (eq) return true;
	    }
	    for (var i=0; i<this.height; i++) {
		var row = this.data[i];
		var nrow = [];
		for (var j=0; j<this.width; j++) {
		    if (fate[j]==-1) continue;
		    nrow[fate[j]] = row[j];
		}
		while (nrow.length<wfate) {
		    nrow.push(null);
		}
		this.data[i] = nrow;
	    }
	    this.width = wfate;
	    return true;
	}

	TableView.prototype.isSimilar = function(alt) {
	    if (alt.get_width()!=this.get_width()) return false;
	    if (alt.get_height()!=this.get_height()) return false;
	    for (var c=0; c<this.width; c++) {
		for (var r=0; r<this.height; r++) {
		    var v1 = "" + this.getCell(c,r);
		    var v2 = "" + alt.getCell(c,r); 
		    if (v1!=v2) {
			console.log("MISMATCH "+ v1 + " " + v2);
			return false;
		    }
		}
	    }
	    return true;
	}

	TableView.prototype.getMeta = function() {
	    return null;
	}

	coopy.TableView = TableView;

	if (typeof exports != "undefined") {
	    exports.CellView = CellView;
	    exports.TableView = TableView;
	    if (typeof exports["daff"] == "undefined") exports["daff"] = exports["coopy"];
	    exports.daff.CellView = TableView;
	    exports.daff.TableView = TableView;
	} else {
	    if (typeof window["daff"] == "undefined") window["daff"] = {};
	    window.daff.CellView = CellView;
	    window.daff.TableView = TableView;
	}

	})();
	(function() {

	var coopy = null;
	if (typeof exports != "undefined") {
	    if (typeof exports.Coopy != "undefined") {
		coopy = exports;
	    }
	}
	if (coopy == null) {
	    coopy = window.daff;
	}


	/**
	 *
	 * Wrapper around a table expressed as rows of hashes.  A mapping function can be passed if the
	 * representation needs to be adapted a little.  The function will be passed data[i] and should
	 * return a simple hash of { "col1": "val1", "col2": "val2", ... }
	 *
	 */
	var NdjsonTable = function(data,mapping) {
	    this.data = data;
	    this.height = data.length;
	    this.width = 0;
	    this.columns = [];
	    this.hasMapping = (mapping!=null);
	    if (mapping==null) {
	        mapping = function(x) { return x };
	    }
	    this.mapping = mapping;
	    var column_name_to_number = {};
	    if (this.height>0) {
	        // We scan all rows to find all fields in use.
	        for (var i=0; i<this.height; i++) {
	            var row = mapping(data[i]);
	            for (var key in row) {
	                if (key in column_name_to_number) continue;
	                if (!row.hasOwnProperty(key)) continue;
	                this.width++;
	                column_name_to_number[key] = this.columns.length;
	                this.columns.push(key);
	            }
	        }
	    }
	    this.columns.sort(); // make order deterministic
	    if (this.height>0) this.height++;
	}

	NdjsonTable.prototype.get_width = function() {
	    return this.width;
	}

	NdjsonTable.prototype.get_height = function() {
	    return this.height;
	}

	NdjsonTable.prototype.getCell = function(x,y) {
	    var key = this.columns[x];
	    if (key == null) throw Error("bad key");
	    if (y==0) return key;
	    return this.mapping(this.data[y-1])[key];
	}

	NdjsonTable.prototype.setCell = function(x,y,c) {
	    var key = this.columns[x];
	    if (key == null && y!=0) throw Error("bad key");
	    if (y==0) {
	        if (key!=null) throw Error("cannot yet change column set in this type of table");
	        this.columns[x] = c;
	    } else {
	        this.mapping(this.data[y-1])[key] = c;
	    }
	}

	NdjsonTable.prototype.toString = function() {
	    return coopy.SimpleTable.tableToString(this);
	}

	NdjsonTable.prototype.getCellView = function() {
	    return new coopy.CellView();
	}

	NdjsonTable.prototype.isResizable = function() {
	    // Ndjson wrapper can't usefully cope with schema changes.
	    return false;
	}

	NdjsonTable.prototype.resize = function(w,h) {
	    return false;
	}

	NdjsonTable.prototype.clear = function() {
	    return false;
	}

	NdjsonTable.prototype.getData = function() {
	    return this.data;
	}

	NdjsonTable.prototype.clone = function() {
	    var ndata = [];
	    for (var i=0; i<this.data.length; i++) {
	        var row = ndata[i] = {};
	        for (var c=0; c<this.columns; c++) {
	            var key = this.columns[c];
		    row[key] = this.data[i][key];
	        }
	    }
	    return new NdjsonTable(ndata,this.hasMapping?this.mapping:null);
	}

	NdjsonTable.prototype.insertOrDeleteRows = function(fate, hfate) {
	    return false;
	}

	NdjsonTable.prototype.insertOrDeleteColumns = function(fate, wfate) {
	    return false;
	}

	NdjsonTable.prototype.getMeta = function() {
	    return null;
	}


	if (typeof exports != "undefined") {
	    exports.NdjsonTable = NdjsonTable;
	} else {
	    if (typeof window["daff"] == "undefined") window["daff"] = {};
	    window.daff.NdjsonTable = NdjsonTable;
	}

	})();
	if (typeof exports != "undefined") {
	    (function() {
		var daff = exports;

		SqliteDatabase = function(db,fname,Fiber) {
		    this.db = db;
	            this.fname = fname;
		    this.row = null;
		    this.active = false;
		    this.index2name = {};
		    this.Fiber = Fiber;
		}

	        SqliteDatabase.prototype.getHelper = function() {
	            return new daff.SqliteHelper();
	        }
		
		SqliteDatabase.prototype.getQuotedColumnName = function (name) {
		    return name;
		}
		
		SqliteDatabase.prototype.getQuotedTableName = function (name) {
		    return name.toString();
		}
		
		SqliteDatabase.prototype.getColumns = function(name) {
		    var fiber = this.Fiber.current;
		    var qname = this.getQuotedColumnName(name);
		    var self = this;
		    this.db.all("pragma table_info("+qname+")", function(err,rows) {
			var lst = [];
			for (var i in rows) {
			    var x = rows[i];
	                    var col = new daff.SqlColumn();
	                    col.setName(x['name']);
	                    col.setPrimaryKey(x['pk']>0);
	                    if (x['type']) {
	                        col.setType(x['type'],'sqlite');
	                    }
			    lst.push(col);
			    self.index2name[i] = x['name'];
			}
			fiber.run(lst);
		    });
		    return this.Fiber.yield();
		}
		
		SqliteDatabase.prototype.exec = function(query,args) {
		    var fiber = this.Fiber.current;
		    if (args==null) {
			this.db.run(query,function(err) {
			    if (err) console.log(err);
			    fiber.run(err==null);
			});
			return this.Fiber.yield();
		    }
		    var statement = this.db.run(query,args,function(err) {
			if (err) console.log(err);
			fiber.run(err==null);
		    });
		    return this.Fiber.yield();
		}
		
		SqliteDatabase.prototype.beginRow = function(tab,row,order) {
		    return this.begin("SELECT * FROM " + this.getQuotedColumnName(tab) + " WHERE rowid = ?",
				      [row],
				      order);
		}
		
		SqliteDatabase.prototype.begin = function(query,args,order) {
		    if (order!=null) {
			this.index2name = {};
			var len = order.length;
			for (var i=0; i<len; i++) {
			    this.index2name[i] = order[i];
			}
		    }
		    var fiber = this.Fiber.current;
		    this.active = true;
		    var self = this;
		    this.db.each(query,(args==null)?[]:args,function(err,row) {
			if (err) {
			    fiber.run([false,0]);
			} else {
			    fiber.run([true,row]);
			}
		    },function(err,n) {
	                if (err) {
	                    console.log(err);
	                }
			fiber.run([false,n]);
		    });
		    return true;
		}

		SqliteDatabase.prototype.read = function() {
		    if (!this.active) return false;
		    var v = this.Fiber.yield();
		    if (v[0]) {
			this.row = v[1];
			return true;
		    }
		    this.row = null;
		    this.active = false;
		    return false;
		}

		SqliteDatabase.prototype.get = function(index) {
		    return this.row[this.index2name[index]];
		}

		SqliteDatabase.prototype.end = function() {
		    while (this.active) {
			this.read();
		    }
		}

		SqliteDatabase.prototype.rowid = function() {
		    return "rowid";
		}

		SqliteDatabase.prototype.getNameForAttachment = function() {
		    return this.fname;
		}

		exports.SqliteDatabase = SqliteDatabase;

	    })();
	}
	if (typeof exports != "undefined") {
	    
	    var tio = {};
	    var tio_args = [];

	    var coopy = exports;
	    var fs = require('fs');
	    var exec = require('child_process').exec;
	    var readline = null;
	    var Fiber = null;
	    var sqlite3 = null;
	    var tty = null;
	    
	    tio.valid = function() {
	        return true;
	    }

	    tio.getContent = function(name) {
	        var txt = "";
		if (name=="-") {
		    // only works on Linux, all other solutions seem broken
		    txt = fs.readFileSync('/dev/stdin',"utf8");
		} else {
		    txt = fs.readFileSync(name,"utf8");
	        }
	        if (txt.charCodeAt(0) === 0xFEFF) {
		    return txt.slice(1);
		}
	        return txt;
	    }
	    
	    tio.saveContent = function(name,txt) {
		return fs.writeFileSync(name,txt,"utf8");
	    }
	    
	    tio.args = function() {
		return tio_args;
	    }
	    
	    tio.writeStdout = function(txt) {
		process.stdout.write(txt);
	    }
	    
	    tio.writeStderr = function(txt) {
		process.stderr.write(txt);
	    }
	    
	    tio.async = function() {
		return true;
	    }

	    tio.exists = function(path) {
		return fs.existsSync(path);
	    }

	    tio.isTtyKnown = function() {
	        return true;
	    }

	    tio.isTty = function() {
	        if (typeof process.stdout.isTTY !== 'undefined') {
	            if (process.stdout.isTTY) return true;
	        } else {
	            // fall back on tty api
	            if (tty==null) tty = require('tty');
	            if (tty.isatty(process.stdout.fd)) return true;
	        }
	        // There's a wrinkle when called from git.  Git may have started a pager that
	        // respects color but which will not be detected as a terminal.  In this case,
	        // it appears that git defines GIT_PAGER_IN_USE, so we watch out for that.
	        if (process.env.GIT_PAGER_IN_USE == 'true') return true;
	        return false;
	    }

	    tio.openSqliteDatabase = function(path) {
		if (Fiber) {
		    return new coopy.SqliteDatabase(new sqlite3.Database(path),path,Fiber);
		}
		throw("run inside Fiber plz");
		return null;
	    }

	    tio.sendToBrowser = function(html) {
	        var http = require("http");
		var shutdown = null;
	        var server = http.createServer(function(request, response) {
	            response.writeHead(200, 
	                               {
	                                   "Content-Type": "text/html; charset=UTF-8",
	                                   "Connection": "close"
	                               });
	            response.write(html);
	            response.end();
		    shutdown();
	        });
		var sockets = {}, nextSocketId = 0;
		server.on('connection', function (socket) {
		    var socketId = nextSocketId++;
		    sockets[socketId] = socket;
		    socket.on('close', function () {
			delete sockets[socketId];
		    });
		});
		shutdown = function() {
		    server.close();
		    for (var socketId in sockets) {
			sockets[socketId].destroy();
		    }
		};
	        server.listen(0,null,null,function() {
	            var target = "http://localhost:" + server.address().port;
	            var exec = require('child_process').exec;
	            var cmd = "xdg-open";
	            switch (process.platform) {
	            case 'darwin':
			cmd = 'open';
			break;
	            case 'win32':
			cmd = 'start ""';
			break;
	            }
	            exec(cmd + ' "' + target + '"', function(error) { 
			if (error) {
	                    console.error(error);
	                    server.close();
			}
	            });
		});
	    }

	    var cmd_result = 1;
	    var cmd_pending = null;

	    tio.command = function(cmd,args) {
		// we promise not to use any arguments with quotes in them
		for (var i=0; i<args.length; i++) {
		    var argi = args[i];
		    if (argi.indexOf(" ")>=0) {
			argi = "\"" + argi + "\"";
		    }
		    cmd += " " + argi;
		}
		var cmd = cmd; + " " + args.join(" ");
		if (cmd == cmd_pending) {
		    cmd_pending = null;
		    return cmd_result;
		} else if (cmd_pending!=null) {
		    return 998; // "hack not working correctly"
		}
		cmd_pending = cmd;
		return 999; // "cannot be executed synchronously"
	    }

	    function run_daff_base(main,args) {
		tio_args = args.slice();
		var code = main.coopyhx(tio);
		if (code==999) {
		    if (cmd_pending!=null) {
			exec(cmd_pending,function(error,stdout,stderr) {
			    cmd_result = 0;
			    if (error!=null) {
				cmd_result = error.code;
			    }
			    return run_daff_base(main,args);
			});
		    }
		} 
		return code;
	    }
	    
	    exports.run_daff_main = function() {
		var main = new exports.Coopy();
		var code = run_daff_base(main,process.argv.slice(2));
		if (code!=999) {
	            if (code!=0) {
		        process.exit(code);
	            }
		}
	    }

	    exports.cmd = function(args) {
		var main = new exports.Coopy();
		var code = run_daff_base(main,args);
		return code;
	    }
	}

	if (typeof require != "undefined") {
	    if (require.main === module) {
		try {
		    exports.run_daff_main();
		} catch (e) {
		    if (e == "run inside Fiber plz") {
			try {
			    Fiber = require('fibers');
			    sqlite3 = require('sqlite3');
			} catch (err) {
			    // We don't have what we need for accessing the sqlite database.
			    console.log("No sqlite3/fibers");
			}
			Fiber(function() {
			    exports.run_daff_main();
			}).run();
	            } else {
	                throw(e);
	            }
		}
	    }
	}
   return exports;
}
